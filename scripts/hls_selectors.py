"""Construction-captured PATH selectors; validate before any guarded subprocess.

This is a launch-time integrity check, not protection against concurrent mutation.
Never capture from PATH or repair a previously constructed selector directory.
"""
import hashlib
import json
import os
from pathlib import Path
import re
import stat


def sha(path):
    h = hashlib.sha256()
    with Path(path).open('rb') as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b''):
            h.update(chunk)
    return h.hexdigest()


def require(ok, message):
    if not ok:
        raise ValueError('tool selectors: ' + message)


def directory(root):
    tools = root / 'tools'
    require(stat.S_ISDIR(tools.lstat().st_mode) and tools.resolve(strict=True) == tools,
            'directory redirected or not a real directory')
    return tools


def capture(root, ghc, cabal):
    """Create the complete mapping from explicit compiler/Cabal inputs, once."""
    tools = directory(root)
    require(not list(tools.iterdir()), 'construction requires empty directory')
    targets = {p.name: p for p in sorted(ghc.parent.iterdir()) if p.is_file()}
    require('cabal' not in targets, 'compiler directory collides with Cabal')
    targets['cabal'] = cabal
    selectors = {}
    for name, target in targets.items():
        resolved = target.resolve(strict=True)
        selectors[name] = {'target': str(target), 'resolved': str(resolved),
                           'sha256': sha(resolved),
                           'mode': stat.S_IMODE(resolved.stat().st_mode)}
        (tools / name).symlink_to(target)
    manifest = {'schema': 1, 'compiler_bin': str(ghc.parent), 'cabal': str(cabal),
                'selectors': selectors}
    path = root / 'inputs/tool-selectors.json'
    with path.open('x') as stream:
        json.dump(manifest, stream, indent=2)
        stream.write('\n')
    path.chmod(0o444)
    validate(root, sha(path), {str(p): sha(p) for p in
                             (ghc, ghc.parent / 'ghc-pkg-9.14.1', cabal)})
    return sha(path)


def validate(root, digest, toolchain):
    tools = directory(root)
    path = root / 'inputs/tool-selectors.json'
    require(not path.is_symlink() and sha(path) == digest, 'construction manifest identity mismatch')
    data = json.loads(path.read_text())
    require(set(data) == {'schema', 'compiler_bin', 'cabal', 'selectors'} and
            type(data['schema']) is int and data['schema'] == 1, 'malformed manifest')
    compiler_bin = Path(data['compiler_bin'])
    cabal = Path(data['cabal'])
    require(compiler_bin.is_absolute() and cabal.is_absolute(), 'nonabsolute tool input')
    selectors = data['selectors']
    require(isinstance(selectors, dict), 'malformed mapping')
    required = {'ghc': compiler_bin / 'ghc-9.14.1',
                'ghc-9.14.1': compiler_bin / 'ghc-9.14.1',
                'ghc-pkg': compiler_bin / 'ghc-pkg-9.14.1',
                'ghc-pkg-9.14.1': compiler_bin / 'ghc-pkg-9.14.1', 'cabal': cabal}
    require(required.keys() <= selectors.keys(), 'required compiler/Cabal aliases missing')
    require(set(p.name for p in tools.iterdir()) == set(selectors), 'missing or unexpected selector')
    for name, entry in selectors.items():
        require(isinstance(name, str) and re.fullmatch(r'[A-Za-z0-9][A-Za-z0-9._+-]*', name),
                'malformed selector name')
        require(isinstance(entry, dict) and set(entry) == {'target', 'resolved', 'sha256', 'mode'},
                'malformed selector record: ' + name)
        target = cabal if name == 'cabal' else compiler_bin / name
        require(entry['target'] == str(target), 'unintended target: ' + name)
        resolved = Path(entry['resolved'])
        require(resolved.is_absolute() and re.fullmatch(r'[0-9a-f]{64}', entry['sha256']) and
                type(entry['mode']) is int and 0 <= entry['mode'] <= 0o7777,
                'malformed target identity: ' + name)
        selector = tools / name
        require(selector.is_symlink() and os.readlink(selector) == entry['target'],
                'link identity mismatch: ' + name)
        require(selector.resolve(strict=True) == resolved, 'resolution mismatch: ' + name)
        require(resolved.is_file() and sha(resolved) == entry['sha256'] and
                stat.S_IMODE(resolved.stat().st_mode) == entry['mode'], 'content/mode mismatch: ' + name)
        if name in required:
            require(resolved == required[name] and str(resolved) in toolchain and
                    toolchain[str(resolved)] == entry['sha256'] and entry['mode'] & 0o111,
                    'absolute tool/alias binding mismatch: ' + name)
    return len(selectors)
