#!/usr/bin/env python3
"""Exercise guarded raw server on an immutable current-byte scratch snapshot.
Requires clean diagnostics + typed hover, detects an in-memory parse error, then
requires recovery on the original bytes. No source file is edited by this probe.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import queue
import subprocess
import sys
import threading
import time


def main():
    ap=argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--root',type=Path,required=True)
    ap.add_argument('--ghc',type=Path,required=True)
    ap.add_argument('--snapshot',type=Path,required=True)
    ap.add_argument('--authority',type=Path,required=True)
    ap.add_argument('--evidence',type=Path,required=True)
    a=ap.parse_args(); a.evidence.mkdir(parents=True,exist_ok=False)
    snapshot=a.snapshot.resolve(strict=True)
    authority=json.loads(a.authority.read_text())
    if str(snapshot)!=authority['snapshot'] or snapshot==Path(authority['source']):raise ValueError('wrong snapshot authority')
    def verify():
        for n,entry in authority['files'].items():
            if hashlib.sha256((snapshot/n).read_bytes()).hexdigest()!=entry['sha256']:raise ValueError('source snapshot changed: '+n)
    verify()
    module=snapshot/'src/Markovian/Horizon.hs'; text=module.read_text(); uri=module.as_uri()
    line=text.splitlines().index('mkHorizon value')
    ledger=(a.evidence/'lsp.jsonl').open('x'); stderr=(a.evidence/'server-stderr.log').open('xb')
    argv=[sys.executable,str(Path(__file__).with_name('guard-hls.py')),'--root',str(a.root),'--ghc',str(a.ghc)]
    (a.evidence/'command.json').write_text(json.dumps({'argv':argv,'cwd':str(snapshot),'authority_sha256':hashlib.sha256(a.authority.read_bytes()).hexdigest(),
        'probe_sha256':hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        'guard_sha256':hashlib.sha256(Path(__file__).with_name('guard-hls.py').read_bytes()).hexdigest(),
        'build_receipt_sha256':hashlib.sha256((a.root/'build-receipt.json').read_bytes()).hexdigest(),
        'runtime_seal_sha256':hashlib.sha256((a.root/'runtime-seal.json').read_bytes()).hexdigest(),
        'started':time.time()},indent=2)+'\n')
    p=subprocess.Popen(argv,cwd=snapshot,stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=stderr)
    inbox=queue.Queue(); lock=threading.Lock(); responses={}; notifications=[]
    def entry(direction,value):
        with lock: ledger.write(json.dumps({'time':time.time(),'direction':direction,'message':value})+'\n');ledger.flush()
    def reader():
        try:
            while True:
                headers={}
                while True:
                    b=p.stdout.readline()
                    if not b:raise EOFError('server stdout closed')
                    if b in (b'\r\n',b'\n'):break
                    k,v=b.decode().split(':',1);headers[k.lower()]=v.strip()
                n=int(headers['content-length']); data=p.stdout.read(n)
                if len(data)!=n:raise EOFError('short LSP message')
                msg=json.loads(data);entry('receive',msg);inbox.put(msg)
        except Exception as exc:inbox.put(exc)
    threading.Thread(target=reader,daemon=True).start()
    def send(method,params=None,id=None):
        msg={'jsonrpc':'2.0','method':method}
        if params is not None:msg['params']=params
        if id is not None:msg['id']=id
        entry('send',msg);data=json.dumps(msg).encode();p.stdin.write(f'Content-Length: {len(data)}\r\n\r\n'.encode()+data);p.stdin.flush()
    def respond(id,result):
        msg={'jsonrpc':'2.0','id':id,'result':result};entry('send',msg);data=json.dumps(msg).encode();p.stdin.write(f'Content-Length: {len(data)}\r\n\r\n'.encode()+data);p.stdin.flush()
    def receive(timeout):
        msg=inbox.get(timeout=timeout)
        if isinstance(msg,Exception):raise msg
        if 'method' in msg and 'id' in msg:
            method=msg['method']
            if method=='workspace/configuration':respond(msg['id'],[{} for _ in msg.get('params',{}).get('items',[])])
            elif method=='workspace/workspaceFolders':respond(msg['id'],[{'uri':snapshot.as_uri(),'name':'Markovian-current-snapshot'}])
            else:respond(msg['id'],None)
        elif 'id' in msg:responses[msg['id']]=msg
        else:notifications.append(msg)
        return msg
    def wait_response(id,timeout=600):
        end=time.monotonic()+timeout
        while id not in responses:receive(max(0.01,end-time.monotonic()))
        msg=responses.pop(id)
        if 'error' in msg:raise RuntimeError('LSP error: '+json.dumps(msg))
        return msg.get('result')
    def wait_diagnostics(want_error,after,timeout=600):
        end=time.monotonic()+timeout
        while True:
            for msg in notifications[after:]:
                if msg.get('method')=='textDocument/publishDiagnostics' and msg['params'].get('uri')==uri:
                    errors=[d for d in msg['params']['diagnostics'] if d.get('severity')==1]
                    if bool(errors)==want_error:return msg['params']
            receive(max(0.01,end-time.monotonic()))
    result={'status':'BLOCKED','abi_check_passed':False,'project_lsp_passed':False,'operational':False}
    try:
        send('initialize',{'processId':os.getpid(),'rootUri':snapshot.as_uri(),'workspaceFolders':[{'uri':snapshot.as_uri(),'name':'Markovian-current-snapshot'}],
                         'capabilities':{'workspace':{'configuration':True},'textDocument':{'hover':{'contentFormat':['plaintext','markdown']},'publishDiagnostics':{'versionSupport':True}}}},id=1)
        init=wait_response(1);result['serverInfo']=init.get('serverInfo');send('initialized',{})
        send('textDocument/didOpen',{'textDocument':{'uri':uri,'languageId':'haskell','version':1,'text':text}})
        send('textDocument/hover',{'textDocument':{'uri':uri},'position':{'line':line,'character':2}},id=2)
        hover=wait_response(2)
        if not hover or 'Integer' not in json.dumps(hover) or 'Horizon' not in json.dumps(hover):raise ValueError('missing typed project hover')
        result['initial_hover']=hover
        # HLS can suppress an initial empty diagnostics publication. Do not
        # equate its absence with success: require error -> clean below instead.
        # Negative control is protocol-only, not a source edit or plugin change.
        marker=len(notifications)
        send('textDocument/didChange',{'textDocument':{'uri':uri,'version':2},'contentChanges':[{'text':text+'\nworkerProbeSyntaxError = )\n'}]})
        result['negative_parse_diagnostics']=wait_diagnostics(True,marker)
        marker=len(notifications)
        send('textDocument/didChange',{'textDocument':{'uri':uri,'version':3},'contentChanges':[{'text':text}]})
        result['recovered_diagnostics']=wait_diagnostics(False,marker)
        send('textDocument/hover',{'textDocument':{'uri':uri},'position':{'line':line,'character':2}},id=3)
        recovered=wait_response(3)
        if not recovered or 'Integer' not in json.dumps(recovered) or 'Horizon' not in json.dumps(recovered):raise ValueError('no recovered typed hover')
        result['recovered_hover']=recovered
        send('textDocument/didClose',{'textDocument':{'uri':uri}})
        send('shutdown',id=4);wait_response(4,60);send('exit');p.stdin.close()
        code=p.wait(timeout=60)
        if code:raise ValueError('server exit '+str(code))
        stderr.flush();server_log=(a.evidence/'server-stderr.log').read_text()
        if '"abi_guard": "PASS"' not in server_log:raise ValueError('guard pass evidence absent')
        verify()
        candidates=[]
        for candidate in (a.root/'cache').rglob('plan.json'):
            data=json.loads(candidate.read_text())
            if data.get('compiler-id') != 'ghc-9.14.1':continue
            roots=[Path(item.get('pkg-src',{}).get('path','/absent')).resolve() for item in data.get('install-plan',[]) if item.get('pkg-name')=='Markovian']
            if snapshot in roots:candidates.append(candidate)
        if len(candidates)!=1:raise ValueError('expected one actual snapshot-bound cradle plan, found '+str(candidates))
        plan=candidates[0]
        plan_bytes=plan.read_bytes();(a.evidence/'project-plan.json').write_bytes(plan_bytes)
        result['project_plan_source']=str(plan)
        result.update(status='PASS',abi_check_passed=True,project_lsp_passed=True,operational=True,server_exit=code,
                      project_plan_sha256=hashlib.sha256(plan_bytes).hexdigest(),snapshot_source_files_unchanged=True)
    except Exception as exc:
        result['error']=repr(exc)
        raise
    finally:
        if p.poll() is None:
            p.terminate()
            try:p.wait(timeout=15)
            except subprocess.TimeoutExpired:p.kill();p.wait()
        stderr.close();ledger.close();result['finished']=time.time()
        (a.evidence/'outcome.json').write_text(json.dumps(result,indent=2)+'\n')
        for f in a.evidence.iterdir():
            if f.is_file():f.chmod(0o444)
        print(json.dumps(result,indent=2))

if __name__=='__main__':main()
