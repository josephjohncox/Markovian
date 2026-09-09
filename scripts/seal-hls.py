#!/usr/bin/env python3
"""Seal a completed fresh build's inputs and actual linked shared objects.
Run once immediately after install-hls-official.py, before guard-hls.py. Keep the
whole absolute build/store tree: the upstream dynamic server is not relocatable.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import time

import hls_selectors

def sha(p):
    h=hashlib.sha256()
    with p.open('rb') as f:
        for b in iter(lambda:f.read(1024*1024),b''):h.update(b)
    return h.hexdigest()
def main():
    ap=argparse.ArgumentParser(description=__doc__);ap.add_argument('--root',type=Path,required=True);a=ap.parse_args()
    root=a.root.resolve(strict=True)
    if (root/'runtime-seal.json').exists():raise ValueError('refusing to replace a runtime seal')
    receipt=json.loads((root/'build-receipt.json').read_text())
    if receipt.get('fresh_build_completed') is not True:raise ValueError('fresh build incomplete')
    inputs={str(p.relative_to(root)):sha(p) for p in sorted((root/'inputs').iterdir()) if p.is_file()}
    if inputs['inputs/install-hls-official.py']!=receipt['installer_sha256'] or inputs['inputs/plan.json']!=receipt['plan_sha256']:raise ValueError('receipt input mismatch')
    files=json.loads((root/'inputs/source-files.json').read_text())
    if len(files)!=1722 or any(sha(root/'source'/n)!=h for n,h in files.items()):raise ValueError('source identity changed')
    tools=json.loads((root/'inputs/toolchain.json').read_text())
    if inputs['inputs/hls_selectors.py'] != receipt['selector_helper_sha256'] or sha(Path(hls_selectors.__file__)) != receipt['selector_helper_sha256']:raise ValueError('selector helper identity mismatch')
    selector_count=hls_selectors.validate(root, receipt['selectors_sha256'], tools)
    for n,h in tools.items():
        if sha(Path(n))!=h:raise ValueError('tool identity changed')
    ghc=next(Path(n) for n in tools if Path(n).name=='ghc-9.14.1')
    env={'PATH':str(root/'tools')+':/usr/bin:/bin','HOME':str(root/'home'),'GHC_ENVIRONMENT':'-'}
    libdir=Path(subprocess.check_output([str(ghc),'--print-libdir'],env=env,text=True).strip())
    backend=libdir.parent/'bin/ghc-9.14.1'
    if not backend.is_file():raise ValueError('GHC backend missing')
    runtime={str(backend):sha(backend)};ldd_logs={}
    for name,h in receipt['outputs'].items():
        binary=root/'install'/name
        if sha(binary)!=h:raise ValueError('output identity mismatch')
        output=subprocess.check_output(['/usr/bin/ldd',str(binary)],env=env,text=True,stderr=subprocess.STDOUT)
        if 'not found' in output:raise ValueError('missing linked library')
        ldd_logs[name]=output
        for line in output.splitlines():
            match=re.search(r'(?:=>\s+)?(/\S+)\s+\(0x[0-9a-f]+\)',line)
            if match:
                p=Path(match.group(1));runtime[str(p)]=sha(p)
    seal={'status':'SEALED','created':time.time(),'seal_script_sha256':sha(Path(__file__)),
          'build_receipt_sha256':sha(root/'build-receipt.json'),'selectors_sha256':receipt['selectors_sha256'],'selector_count':selector_count,'inputs':inputs,'runtime_files':runtime,'ldd':ldd_logs,
          'relocation':'not supported; preserve absolute build/store paths','original_source_files_verified':1722}
    with (root/'runtime-seal.json').open('x') as f:json.dump(seal,f,indent=2);f.write('\n')
    (root/'runtime-seal.json').chmod(0o444)
    print(json.dumps({'runtime_files':len(runtime),'runtime_seal_sha256':sha(root/'runtime-seal.json')}))
if __name__=='__main__':main()
