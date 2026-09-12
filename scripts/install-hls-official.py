#!/usr/bin/env python3
"""Fresh isolated official HLS 2.14 build; never installs globally or reuses a store.
Only upstream bounds policy plus the approved ABI-flag strengthening is accepted.
A built receipt is NOT an operational acceptance receipt; run guard-hls.py too.
"""
import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path, PurePosixPath
import shutil
import subprocess
import sys
import tarfile
import time

import hls_selectors

URL = 'https://downloads.haskell.org/~hls/haskell-language-server-2.14.0.0/haskell-language-server-2.14.0.0-src.tar.gz'
SHA = 'ee8e2007d3ff98bcc0d1c5409092d69c3f176b8419b85b31a4dccd22b45914f6'
PROJECT_SHA = '7ff452b79b0b702529922c4e40aae9c2052c19faf25ccbef854aeaebd5dca4c9'
TOP = 'haskell-language-server-2.14.0.0'
INDEX = '2026-04-16T00:00:00Z'
TARGETS = ['exe:haskell-language-server', 'exe:haskell-language-server-wrapper']

def sha(path):
    h = hashlib.sha256()
    with Path(path).open('rb') as f:
        for b in iter(lambda: f.read(1024*1024), b''): h.update(b)
    return h.hexdigest()

def record(path, value):
    with Path(path).open('x') as f: json.dump(value, f, indent=2); f.write('\n')
    Path(path).chmod(0o444)

def strengthen(data):
    if hashlib.sha256(data).hexdigest() != PROJECT_SHA:
        raise ValueError('unexpected upstream cabal.project identity')
    old = b'ghc-check -ghc-check-use-package-abis'
    if data.count(old) != 1: raise ValueError('ABI flag not unique')
    return data.replace(old, b'ghc-check +ghc-check-use-package-abis')

def extract(archive, dest):
    if sha(archive) != SHA: raise ValueError('official source SHA256 mismatch')
    manifest = {}
    with tarfile.open(archive, 'r:gz') as t:
        members = t.getmembers()
        names = set()
        for m in members:
            p = PurePosixPath(m.name)
            if p.is_absolute() or '..' in p.parts or not p.parts or p.parts[0] != TOP or m.name in names:
                raise ValueError('unsafe/duplicate archive path: ' + m.name)
            names.add(m.name)
            if not (m.isfile() or m.isdir() or m.issym()): raise ValueError('unsupported archive member')
            if m.issym() and (m.name != TOP+'/docs/contributing/plugin-tutorial.lhs' or m.linkname != 'plugin-tutorial.md'):
                raise ValueError('unexpected archive symlink')
        links = {m.name for m in members if m.issym()}
        for m in members:
            if any(str(p) in links for p in PurePosixPath(m.name).parents): raise ValueError('symlink ancestor')
        # Explicit extraction: no tar permission/owner changes or link traversal.
        for m in members:
            p = dest / m.name
            if m.isdir(): p.mkdir(parents=True, exist_ok=True)
            elif m.isfile():
                p.parent.mkdir(parents=True, exist_ok=True)
                with p.open('xb') as f: shutil.copyfileobj(t.extractfile(m), f)
                p.chmod(0o555 if m.mode & 0o111 else 0o444)
                manifest[m.name] = sha(p)
        for m in members:
            if m.issym(): (dest/m.name).symlink_to(m.linkname)
    if len(manifest) != 1722: raise ValueError('source regular-file count differs')
    return manifest

def environment(root, ghc):
    # Allowlisted environment: do not inherit Cabal/GHC/Stack flags or user config.
    e = {k: os.environ[k] for k in ('LANG', 'LC_ALL', 'TZ') if k in os.environ}
    e.update(HOME=str(root/'home'), CABAL_DIR=str(root/'cabal'), CABAL_CONFIG=str(root/'cabal/config'),
             TMPDIR=str(root/'tmp'), XDG_CACHE_HOME=str(root/'cache'), XDG_CONFIG_HOME=str(root/'config'),
             XDG_DATA_HOME=str(root/'data'), XDG_STATE_HOME=str(root/'state'), STACK_ROOT=str(root/'stack'),
             GHCUP_INSTALL_BASE_PREFIX=str(root/'home'), GHC_BIN=str(ghc), GHC_ENVIRONMENT='-',
             PATH=str(root/'tools')+':/usr/bin:/bin')
    return e

def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--root', required=True, type=Path, help='new absent absolute scratch directory')
    ap.add_argument('--ghc', required=True, type=Path)
    ap.add_argument('--cabal', required=True, type=Path)
    ap.add_argument('--archive', type=Path, help='optional cached OFFICIAL archive, SHA256 verified')
    ap.add_argument('--jobs', type=int, default=8)
    a = ap.parse_args()
    if not a.root.is_absolute() or (a.root.exists() or a.root.is_symlink()) or not a.ghc.is_absolute() or not a.cabal.is_absolute() or not 1 <= a.jobs <= 20:
        ap.error('require absent absolute root, absolute tool paths, and 1..20 jobs')
    root = a.root.resolve(); ghc = a.ghc.resolve(strict=True); cabal = a.cabal.resolve(strict=True)
    pkg = ghc.parent/'ghc-pkg-9.14.1'
    if not pkg.is_file(): ap.error('missing sibling exact ghc-pkg-9.14.1')
    root.mkdir(parents=True)
    for d in ('home','cabal','tmp','cache','config','data','state','stack','tools','inputs','evidence','install'):
        (root/d).mkdir()
    tools = {str(p):sha(p) for p in (ghc,pkg,cabal)}
    selector_digest = hls_selectors.capture(root, ghc, cabal)
    shutil.copyfile(hls_selectors.__file__, root/'inputs/hls_selectors.py')
    (root/'inputs/hls_selectors.py').chmod(0o444)
    selector_helper_digest = sha(root/'inputs/hls_selectors.py')
    env = environment(root, ghc)
    config = ('repository hackage.haskell.org\n  url: https://hackage.haskell.org/\n  secure: True\n'
              f'remote-repo-cache: {root}/cabal/packages\nstore-dir: {root}/store\nlogs-dir: {root}/logs\n')
    (root/'cabal/config').write_text(config); (root/'cabal/config').chmod(0o444)
    shutil.copyfile(__file__, root/'inputs/install-hls-official.py'); (root/'inputs/install-hls-official.py').chmod(0o444)
    seq = 0
    def run(label, argv, cwd=root):
        nonlocal seq
        hls_selectors.validate(root, selector_digest, tools)
        seq += 1; stem = root/'evidence'/f'{seq:02d}-{label}'
        record(str(stem)+'.command.json', {'argv':list(map(str,argv)), 'cwd':str(cwd), 'environment':env, 'start':time.time()})
        log = Path(str(stem)+'.log')
        with log.open('xb') as f: result = subprocess.run(list(map(str,argv)),cwd=cwd,env=env,stdout=f,stderr=subprocess.STDOUT)
        log.chmod(0o444)
        record(str(stem)+'.result.json', {'exit':result.returncode,'end':time.time(),'log_sha256':sha(log)})
        print(label, result.returncode, flush=True)
        if result.returncode: raise RuntimeError(f'{label}: exit {result.returncode}; see {log}')
        return log.read_text()
    receipt = {'status':'blocked','fresh_build_completed':False,'abi_check_passed':False,'project_lsp_passed':False,'operational':False,
               'selectors_sha256':selector_digest,'selector_helper_sha256':selector_helper_digest}
    try:
        if run('ghc-version',[ghc,'--numeric-version']).strip() != '9.14.1': raise ValueError('GHC version mismatch')
        if run('cabal-version',[cabal,'--numeric-version']).strip() != '3.18.1.0': raise ValueError('Cabal version mismatch')
        run('ghc-info',[ghc,'--info'])
        db = run('boot-package-db',[pkg,'--global','--no-user-package-db','dump'])
        (root/'inputs/boot-package-db.txt').write_text(db); (root/'inputs/boot-package-db.txt').chmod(0o444)
        record(root/'inputs/toolchain.json', tools)
        archive = root/'inputs/hls-src.tar.gz'
        if a.archive: shutil.copyfile(a.archive,archive)
        else: run('download',['curl','--fail','--location','--proto','=https','--tlsv1.2','--output',archive,URL])
        archive.chmod(0o444)
        manifest = extract(archive,root/'source')
        record(root/'inputs/source-files.json',manifest)
        source = root/'source'/TOP
        original = (source/'cabal.project').read_bytes(); generated = strengthen(original)
        project = source/'cabal.project.abi'
        project.write_bytes(generated); project.chmod(0o444)
        diff = ''.join(difflib.unified_diff(original.decode().splitlines(True),generated.decode().splitlines(True),fromfile='official/cabal.project',tofile='generated/cabal.project.abi'))
        (root/'inputs/project.diff').write_text(diff); (root/'inputs/project.diff').chmod(0o444)
        record(root/'inputs/policy.json',{'url':URL,'archive_sha256':SHA,'source_count':1722,'index_state':INDEX,
              'original_project_sha256':PROJECT_SHA,'generated_project_sha256':sha(project),'only_recipe_change':'ghc-check -ghc-check-use-package-abis -> +ghc-check-use-package-abis',
              'targets':TARGETS,'tests_benchmarks':'disabled by CLI; not tested',
              'upstream_ghc_9_14_excluded_plugins':['HLint','Fourmolu','Ormolu','stylish-haskell','Retrie','Stan','Splice']})
        run('secure-update',[cabal,'update'])
        common = [f'--project-file={project}',f'--builddir={root}/build',f'--with-compiler={ghc}',f'--with-hc-pkg={pkg}', '--disable-tests','--disable-benchmarks']
        run('dependency-plan',[cabal,'build',*common,*TARGETS,'--dry-run','-v2'],source)
        shutil.copyfile(root/'build/cache/plan.json',root/'inputs/plan.json'); (root/'inputs/plan.json').chmod(0o444)
        plan = json.loads((root/'inputs/plan.json').read_text())
        checks = [p for p in plan['install-plan'] if p.get('pkg-name') == 'ghc-check']
        if not all(p.get('flags',{}).get('ghc-check-use-package-abis') is True for p in checks):
            raise ValueError('dependency plan disabled ABI guard')
        record(root/'inputs/abi-guard-construction.json', {
            'ghc_check_components_in_plan': len(checks),
            'external_guard_required': True,
            'reason': 'Raw Cabal server/wrapper targets do not depend on ghc-check; full build-time boot DB and executable identity must be checked by guard-hls.py before execution.'})
        run('fresh-build',[cabal,'build',*common,*TARGETS,f'-j{a.jobs}','-v1'],source)
        outputs = {}
        for target in TARGETS:
            built = Path(run('locate-'+target.split(':')[1],[cabal,'list-bin',*common,target],source).strip())
            if not built.resolve().is_relative_to(root/'build'): raise ValueError('binary escaped fresh build tree')
            dest = root/'install'/target.split(':')[1]
            shutil.copyfile(built,dest); dest.chmod(0o555); outputs[dest.name] = sha(dest)
        if any(sha(root/'source'/p) != h for p,h in manifest.items()): raise ValueError('original source changed')
        if sha(project) != hashlib.sha256(generated).hexdigest(): raise ValueError('generated policy changed')
        if run('final-boot-package-db',[pkg,'--global','--no-user-package-db','dump']) != db: raise ValueError('boot ABI changed during build')
        receipt.update(status='built-not-operationally-accepted',fresh_build_completed=True,outputs=outputs,plan_sha256=sha(root/'inputs/plan.json'))
    except Exception as exc:
        receipt['error'] = str(exc)
        raise
    finally:
        receipt.update(finished=time.time(),installer_sha256=sha(root/'inputs/install-hls-official.py'))
        record(root/'build-receipt.json',receipt)

if __name__ == '__main__': main()
