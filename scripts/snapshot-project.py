#!/usr/bin/env python3
"""Copy one stable, dirty current-source inventory; never copy worktree caches/.git.
Metadata says observation, not a commit-only snapshot or final-writer acceptance.
"""
import hashlib
import json
import os
from pathlib import Path, PurePosixPath
import subprocess
import sys
import time

source = Path(sys.argv[1]).resolve(strict=True)
dest = Path(sys.argv[2]).absolute()
metadata = Path(sys.argv[3]).absolute()
if dest.exists() or metadata.exists(): raise SystemExit('destination must be absent')
def git(*args): return subprocess.check_output(['git','--no-optional-locks','-C',str(source),*args])
def inventory():
    names = sorted(set(git('ls-files','--cached','--others','--exclude-standard','-z').decode().split('\0'))-{''})
    out = {}
    for n in names:
        p = source/n
        if PurePosixPath(n).is_absolute() or '..' in PurePosixPath(n).parts or p.is_symlink() or not p.is_file():
            raise ValueError('unexpected source entry: '+n)
        out[n] = {'sha256':hashlib.sha256(p.read_bytes()).hexdigest(),'mode':p.stat().st_mode & 0o777}
    return {'head':git('rev-parse','HEAD').decode().strip(),'index_sha256':hashlib.sha256(git('ls-files','--stage','-z')).hexdigest(),
            'status':git('status','--porcelain=v1').decode(),'files':out}
before = inventory(); started=time.time(); dest.mkdir(parents=True)
for n,entry in before['files'].items():
    p=dest/n; p.parent.mkdir(parents=True,exist_ok=True)
    data=(source/n).read_bytes()
    if hashlib.sha256(data).hexdigest()!=entry['sha256']: raise ValueError('writer changed source during capture: '+n)
    p.write_bytes(data); p.chmod(0o555 if entry['mode'] & 0o111 else 0o444)
after=inventory()
if before!=after: raise ValueError('active writer changed source inventory; discard and retry absent snapshot destination')
result={'authority':'Observed current tracked + non-ignored untracked bytes, stable before/copy/after; NOT final integration state; another worker owns tracked changes.',
        'source':str(source),'snapshot':str(dest),'started':started,'finished':time.time(),**before}
with metadata.open('x') as f:json.dump(result,f,indent=2);f.write('\n')
metadata.chmod(0o444)
print(json.dumps({'files':len(before['files']),'head':before['head'],'metadata':str(metadata)}))
