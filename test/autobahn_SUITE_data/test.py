#!/usr/bin/env python
import os
import os.path
import sys
import subprocess


AB_TESTS_ENV = os.getenv("AB_TESTS_ENV")
AB_TESTS_PRIV = os.getenv("AB_TESTS_PRIV")

VIRTUALENV_URL = 'https://raw.github.com/pypa/virtualenv/master/virtualenv.py'
VIRTUALENV_BIN = os.path.join(AB_TESTS_ENV, "virtualenv.py")
PIP_BIN = os.path.join(AB_TESTS_ENV, "bin", "pip")


def activate_env(env):
    """
    See 'Using Virtualenv without bin/python' at http://www.virtualenv.org
    """
    activate_this = os.path.join(env, 'bin', 'activate_this.py')
    exec(compile(open(activate_this).read(), activate_this, 'exec'),
        dict(__file__=activate_this))

def install_env(env):
    """
    Install a new virtualenv at a path and also install the Autobahn package.
    """
    os.makedirs(env) if not os.path.isdir(env) else None
    subprocess.check_call(["curl", "-sS", VIRTUALENV_URL, "-o", VIRTUALENV_BIN])
    subprocess.check_call(["python", VIRTUALENV_BIN, env])
    activate_env(env)
    subprocess.check_call([PIP_BIN, "install", "AutobahnTestSuite"])

def client_config():
    """
    See comment on SUPPORTED_SPEC_VERSIONS in Autobahn/.../websocket.py
    """
    base = {
        'options': {'failByDrop': False},
        'enable-ssl': False,
        'servers': [{
             'agent': 'Cowboy/10',
             'url': 'ws://localhost:33080/echo',
             'options': {'version': 10}}, # hybi-10
            {'agent': 'Cowboy/18',
             'url': 'ws://localhost:33080/echo',
             'options': {'version': 18}} # RFC6455
        ],
        'cases': ['*'],
        'exclude-cases': [] }
    return base

def run_test(env, config):
    activate_env(env)
    from twisted.python import log
    from twisted.internet import reactor
    from autobahntestsuite.fuzzing import FuzzingClientFactory
    os.chdir(AB_TESTS_PRIV)
    log.startLogging(sys.stdout)
    fuzzer = FuzzingClientFactory(config)
    return reactor.run()


def main():
    cmd = sys.argv[1]
    if cmd == 'setup':
        install_env(AB_TESTS_ENV)
        print('AB-TESTS-SETUP-OK')
    elif cmd == 'test':
        run_test(AB_TESTS_ENV, client_config())
        print('AB-TESTS-TEST-OK')
    else:
        return 1

if __name__ == '__main__':
    main()
