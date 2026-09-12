#!/usr/bin/env python3
"""Fail closed on full boot-package DB, exact tool/plan/server/source-policy identity.
This is REQUIRED for raw Cabal executables: ghc-check is not in these targets.
--check-only proves the external ABI guard, not project LSP operation.
Without --check-only, exec the guarded raw server in LSP mode.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys

import hls_selectors

SOURCE_SHA = 'ee8e2007d3ff98bcc0d1c5409092d69c3f176b8419b85b31a4dccd22b45914f6'
PROJECT_SHA = '7ff452b79b0b702529922c4e40aae9c2052c19faf25ccbef854aeaebd5dca4c9'

def sha(path):
    h = hashlib.sha256()
    with Path(path).open('rb') as f:
        for b in iter(lambda: f.read(1024*1024), b''): h.update(b)
    return h.hexdigest()

def require(ok, text):
    if not ok: raise ValueError(text)

def check(root, ghc):
    root = root.resolve(strict=True); ghc = ghc.resolve(strict=True)
    receipt = json.loads((root/'build-receipt.json').read_text())
    require(receipt.get('fresh_build_completed') is True, 'no completed fresh build')
    require(sha(root/'inputs/install-hls-official.py') == receipt['installer_sha256'], 'installer hash mismatch')
    tools = json.loads((root/'inputs/toolchain.json').read_text())
    require(str(ghc) in tools and ghc.name == 'ghc-9.14.1', 'wrong compiler input')
    for p,h in tools.items(): require(sha(p) == h, 'tool identity mismatch: '+p)
    require(sha(root/'inputs/hls_selectors.py') == receipt['selector_helper_sha256'] == sha(hls_selectors.__file__), 'selector helper identity mismatch')
    selector_count = hls_selectors.validate(root, receipt['selectors_sha256'], tools)
    seal = json.loads((root/'runtime-seal.json').read_text())
    require(seal.get('seal_script_sha256') == sha(Path(__file__).with_name('seal-hls.py')),
            'runtime seal producer identity mismatch')
    require(seal['selectors_sha256'] == receipt['selectors_sha256'] and
            seal['inputs']['inputs/tool-selectors.json'] == receipt['selectors_sha256'] and
            seal['selector_count'] == selector_count and
            seal['build_receipt_sha256'] == sha(root/'build-receipt.json'), 'selector seal/receipt mismatch')
    pkg = ghc.parent/'ghc-pkg-9.14.1'
    require(str(pkg) in tools, 'ghc-pkg not bound to receipt')
    source = root/'source/haskell-language-server-2.14.0.0'
    require(sha(root/'inputs/hls-src.tar.gz') == SOURCE_SHA, 'archive identity mismatch')
    original = (source/'cabal.project').read_bytes()
    require(hashlib.sha256(original).hexdigest() == PROJECT_SHA, 'upstream project changed')
    require((source/'cabal.project.abi').read_bytes() == original.replace(b'ghc-check -ghc-check-use-package-abis',b'ghc-check +ghc-check-use-package-abis'), 'generated project policy mismatch')
    require(not (source/'cabal.project.abi.local').exists(), 'unexpected local override')
    require(not (source/'cabal.project.abi.freeze').exists(), 'unexpected freeze override')
    require(sha(root/'inputs/plan.json') == receipt['plan_sha256'], 'dependency plan mismatch')
    plan = json.loads((root/'inputs/plan.json').read_text())
    for p in plan['install-plan']:
        if p.get('pkg-name') == 'ghc-check':
            require(p.get('flags',{}).get('ghc-check-use-package-abis') is True, 'ABI guard disabled in plan')
    boot = (root/'inputs/boot-package-db.txt').read_bytes()
    evidence = root/'evidence/04-boot-package-db'
    require((Path(str(evidence)+'.log')).read_bytes() == boot, 'boot manifest differs from actual capture')
    require(hashlib.sha256(boot).hexdigest() == json.loads(Path(str(evidence)+'.result.json').read_text())['log_sha256'], 'boot capture hash mismatch')
    # Deliberately allowlist runtime environment too; no caller GHC/Cabal flags.
    env = {k:os.environ[k] for k in ('LANG','LC_ALL','TZ') if k in os.environ}
    env.update(HOME=str(root/'home'), CABAL_DIR=str(root/'cabal'), CABAL_CONFIG=str(root/'cabal/config'),
               TMPDIR=str(root/'tmp'), XDG_CACHE_HOME=str(root/'cache'), XDG_CONFIG_HOME=str(root/'config'),
               XDG_DATA_HOME=str(root/'data'), XDG_STATE_HOME=str(root/'state'), STACK_ROOT=str(root/'stack'),
               GHCUP_INSTALL_BASE_PREFIX=str(root/'home'), GHC_BIN=str(ghc), GHC_ENVIRONMENT='-',
               PATH=str(root/'tools')+':/usr/bin:/bin')
    config = ('repository hackage.haskell.org\n  url: https://hackage.haskell.org/\n  secure: True\n'
              f'remote-repo-cache: {root}/cabal/packages\nstore-dir: {root}/store\nlogs-dir: {root}/logs\n')
    require((root/'cabal/config').read_text() == config, 'isolated config policy mismatch')
    server = root/'install/haskell-language-server'
    for name,h in receipt['outputs'].items():
        require(name in ('haskell-language-server','haskell-language-server-wrapper'), 'unexpected executable')
        require(sha(root/'install'/name) == h, 'built executable hash mismatch: '+name)
    require('haskell-language-server' in receipt['outputs'], 'server output missing')
    require(seal.get('status') == 'SEALED' and seal.get('original_source_files_verified') == 1722, 'runtime seal incomplete')
    require(seal['build_receipt_sha256'] == sha(root/'build-receipt.json'), 'runtime seal receipt mismatch')
    require(bool(seal['runtime_files']), 'runtime library identity missing')
    for name,h in seal['inputs'].items():
        p = root/name
        require(p.resolve().is_relative_to(root/'inputs'), 'sealed input escaped inputs directory')
        require(sha(p) == h, 'sealed input mismatch: '+name)
    for name,h in seal['runtime_files'].items():
        require(sha(Path(name)) == h, 'linked library/compiler backend identity mismatch: '+name)
    current = subprocess.check_output([str(pkg),'--global','--no-user-package-db','dump'],env=env)
    require(current == boot, 'BOOT PACKAGE ABI/identity mismatch; refusing to launch')
    proof = {'selectors_sha256':receipt['selectors_sha256'],'selectors_verified':selector_count,
             'abi_guard':'PASS','guard':'external full boot DB equality (includes ABI, IDs, versions, library paths)',
             'boot_manifest_sha256':hashlib.sha256(boot).hexdigest(), 'boot_abi_count':boot.count(b'\nabi: '),
             'server_sha256':sha(server),'ghc':str(ghc),'plan_sha256':receipt['plan_sha256'],
             'runtime_seal_sha256':sha(root/'runtime-seal.json'),'runtime_files_verified':len(seal['runtime_files'])}
    return server, env, proof

def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--root',type=Path,required=True)
    p.add_argument('--ghc',type=Path,required=True)
    p.add_argument('--check-only',action='store_true')
    a = p.parse_args()
    try:
        server, env, proof = check(a.root,a.ghc)
        print(json.dumps(proof,sort_keys=True),file=sys.stderr,flush=True)
        if not a.check_only: os.execve(server,[str(server),'--lsp'],env)
    except Exception as exc:
        print('HLS GUARD BLOCKED: '+str(exc),file=sys.stderr)
        return 1
    return 0

if __name__ == '__main__': sys.exit(main())
