import subprocess

# Implemented based on
# https://dev.to/ahferroin7/skip-ci-stages-in-travis-based-on-what-files-changed-3a4k

"""
Env = {
    commitRange: os.environ['TRAVIS_COMMIT_RANGE']
    branch: os.environ['TRAVIS_BRANCH']
    pullRequest: os.environ['TRAVIS_PULL_REQUEST']
    pullRequestBranch: os.environ['TRAVIS_PULL_REQUEST_BRANCH']
    tag: os.environ['TRAVIS_TAG']
    stage: os.environ['TRAVIS_BUILD_STAGE_NAME']
}
"""

"""
returns None when all files should be considered as changed
"""
def getFiles(env):
    if isNewBranch(env):
        return []
    else:
        if isPullRequest(env):
            return getWhenPullRequest(env)
        else:
            if historyExists(env):
                return getWhenPushExisting(env)
            else:
                return None

def execGitDiff(cmd):
    print(cmd)
    out = subprocess.check_output(cmd).decode('utf-8')
    return filter(None, out.split('\n'))

def parseRange(commitRange):
    return commitRange.split('...')

def isPullRequest(env):
    return (env.pullRequest != "false")

def isNewBranch(env):
    return (env.commitRange == "")

def historyExists(env):
    def exists(commit):
        cmd = "git cat-file -t "+str(commit)
        out = subprocess.check_output(cmd).decode('utf-8')
        return (out == 'commit')
    commit1, commit2 = parseRange(env.commitRange)
    return (exists(commit1) and exists(commit2))

def getWhenPullRequest(env):
    return getDiff(env.pullRequestBranch, env.branch)

def getWhenPushExisting(env):
    return getByRange(env.commitRange)

def getDiff(origin, target):
    cmd = "git diff --name-only "+str(origin)+"..."+str(target)
    return execGitDiff(cmd)

def getByRange(commitRange):
    cmd = "git diff --name-only "+str(commitRange)
    return execGitDiff(cmd)
