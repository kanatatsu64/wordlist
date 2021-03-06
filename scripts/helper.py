import sys
import re
import yaml
import subprocess
from logger import log

def load(path):
    config = loadFile(path)
    if not validate(config):
        log('invalid config file')
        exit(1)
    return convert(config)

def loadFile(path):
    with open(path, "r") as f:
        return yaml.load(f)

def validate(config):
    def isDict(val):
        return (type(val) == dict)

    def isString(val):
        return (type(val) == str)

    def validateRecord(rec):
        def isRegexpr(val):
            pat = re.compile(r'^/.*/$')
            return bool(
                isString(val) and \
                pat.search(val)
            )

        keys = ['test', 'script']

        return bool(
            isDict(rec) and \
            sorted(rec.keys()) == sorted(keys) and \
            isRegexpr(rec['test']) and \
            isString(rec['script'])
        )
    
    def validateConfig(config):
        result = True
        for name, rec in config.items():
            result = bool(
                result and \
                isString(name) and \
                validateRecord(rec)
            )
        return result
    
    return bool(
        isDict(config) and \
        validateConfig(config)
    )

def convert(config):
    def compileRegexpr(str):
        pat = re.compile(r'^/(.*)/$')
        regexpr = pat.search(str).group(1)
        return re.compile(regexpr)
    def convertSection(section):
        name, rec = section
        test = rec['test']
        script = rec['script']
        return {
            'name': name,
            'test': compileRegexpr(test),
            'script': script
        }
    def fst(section):
        return section[0]

    sections = sorted(config.items(), key=fst)
    return list(map(convertSection, sections))

def notice(name):
    return "echo 'start "+str(name)+" script'; "

def buildScript(pairs):
    uniqs = {}
    for name, script in pairs:
        if script in uniqs:
            uniqs[script].add(name)
        else:
            uniqs[script] = {name}
    result = []
    for script, names in uniqs.items():
        notices = ''.join(map(notice, sorted(names)))
        result.append(notices + script)
    return result

def getScripts(config, files):
    pairs = []
    for file in files:
        for rec in config:
            name = rec['name']
            test = rec['test']
            script = rec['script']
            if test.search(file):
                pairs.append([name, script])
    return buildScript(pairs)

def getAllScripts(config):
    pairs = []
    for rec in config:
        name = rec['name']
        script = rec['script']
        pairs.append([name, script])
    return buildScript(pairs)

def execCmdOut(cmd, check=False):
    log(cmd)
    try:
        opts = {
            'shell': True,
            'check': True,
            'stdout': subprocess.PIPE
        }
        ret = subprocess.run(cmd, **opts)
        if ret.stdout:
            out = ret.stdout.decode('utf-8')
        else:
            out = ""
    except Exception as e:
        if check:
            raise e
        else:
            log("failed: "+str(cmd))
            log(e)
            return ''
    if out:
        log(out)
    return out 

def execCmd(cmd, check=False):
    log(cmd)
    try:
        opts = {
            'shell': True,
            'check': True
        }
        subprocess.run(cmd, **opts)
    except Exception as e:
        if check:
            raise e
        else:
            log("failed: "+str(cmd))
            log(e)
            return ''

def execScripts(scripts):
    result = True
    for script in scripts:
        try:
            execCmd(script, check=True)
        except Exception as e:
            log("failed: "+str(script))
            log(e)
            result = False
    return result
