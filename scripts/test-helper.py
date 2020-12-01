#!/usr/bin/python3
import unittest
import yaml
import re
from helper import validate, convert, getScripts, getAllScripts, notice

class TestValidate(unittest.TestCase):
    def testValidate1(self):
        yamlStr = '\n'.join([
            "{",
            "  example: {",
            "    test: /^src//,",
            "    script: stack test,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)

        expected = True
        actual = validate(configRaw)
        self.assertEqual(expected, actual)

    def testValidate2(self):
        yamlStr = '\n'.join([
            "{",
            "  example1: {",
            "    test: /^src//,",
            "    script: stack test,",
            "  },",
            "  example2: {",
            "    test: /^src//,",
            "    script: stack test,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)

        expected = True
        actual = validate(configRaw)
        self.assertEqual(expected, actual)

    def testValidate3(self):
        yamlStr = '\n'.join([
            "{",
            "  example1: {",
            "    tests: /^src//,",
            "    script: stack test,",
            "  },",
            "  example2: {",
            "    test: /^src//,",
            "    script: stack test,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)

        expected = False
        actual = validate(configRaw)
        self.assertEqual(expected, actual)

    def testValidate5(self):
        yamlStr = '\n'.join([
            "{",
            "  example1: {",
            "    test: ^src/,",
            "    script: stack test,",
            "  },",
            "  example2: {",
            "    test: /^src//,",
            "    script: stack test,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)

        expected = False
        actual = validate(configRaw)
        self.assertEqual(expected, actual)

    def testValidate6(self):
        yamlStr = '\n'.join([
            "{",
            "  example1: {",
            "    test: /^src//,",
            "    scripts: stack test,",
            "  },",
            "  example2: {",
            "    test: /^src//,",
            "    script: stack test,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)

        expected = False
        actual = validate(configRaw)
        self.assertEqual(expected, actual)

    def testValidate7(self):
        yamlStr = '\n'.join([
            "{",
            "  example1: {",
            "    script: stack test,",
            "  },",
            "  example2: {",
            "    test: /^src//,",
            "    script: stack test,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)

        expected = False
        actual = validate(configRaw)
        self.assertEqual(expected, actual)

    def testValidate8(self):
        yamlStr = '\n'.join([
            "{",
            "  example1: {",
            "    test: /^src//,",
            "  },",
            "  example2: {",
            "    test: /^src//,",
            "    script: stack test,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)

        expected = False
        actual = validate(configRaw)
        self.assertEqual(expected, actual)

    def testValidate9(self):
        yamlStr = '\n'.join([
            "{",
            "  example1: {",
            "    test: /^src//,",
            "    script: stack test,",
            "    other: additional,",
            "  },",
            "  example2: {",
            "    test: /^src//,",
            "    script: stack test,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)

        expected = False
        actual = validate(configRaw)
        self.assertEqual(expected, actual)

class TestConvert(unittest.TestCase):
    def testConvert1(self):
        yamlStr = '\n'.join([
            "{",
            "  example1: {",
            "    test: /^src//,",
            "    script: stack test,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)
        config = convert(configRaw)

        actual = len(config)
        expected = 1
        self.assertEqual(expected, actual)

        expected = ['name', 'test', 'script']
        actual = config[0].keys()
        self.assertCountEqual(expected, actual)

        expected = 'example1'
        actual = config[0]['name']
        self.assertEqual(expected, actual)

        expected = re.compile('^src/')
        actual = config[0]['test']
        self.assertEqual(expected, actual)

        expected = 'stack test'
        actual = config[0]['script']
        self.assertEqual(expected, actual)

    def testConvert2(self):
        yamlStr = '\n'.join([
            "{",
            "  example1: {",
            "    test: /^src1//,",
            "    script: stack test 1,",
            "  },",
            "  example2: {",
            "    test: /^src2//,",
            "    script: stack test 2,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)
        config = convert(configRaw)

        actual = len(config)
        expected = 2
        self.assertEqual(expected, actual)

        for i, rec in enumerate(config):
            expected = ['name', 'test', 'script']
            actual = rec.keys()
            self.assertCountEqual(expected, actual)

            expected = f"example{i+1}"
            actual = rec['name']
            self.assertCountEqual(expected, actual)

            expected = re.compile(f"^src{i+1}/")
            actual = rec['test']
            self.assertEqual(expected, actual)

            expected = f"stack test {i+1}"
            actual = rec['script']
            self.assertEqual(expected, actual)

class TestGetScripts(unittest.TestCase):
    def testGetScripts1(self):
        yamlStr = '\n'.join([
            "{",
            "  example: {",
            "    test: /src/,",
            "    script: stack test,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)
        config = convert(configRaw)
        files = ['base/src/path.hs']

        name = config[0]['name']
        expected = [notice(name) + 'stack test']
        actual = getScripts(config, files)
        self.assertCountEqual(expected, actual)

    def testGetScripts2(self):
        yamlStr = '\n'.join([
            "{",
            "  example: {",
            "    test: /src//,",
            "    script: stack test,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)
        config = convert(configRaw)
        files = ['base/src/path.hs']

        name = config[0]['name']
        expected = [notice(name) + 'stack test']
        actual = getScripts(config, files)
        self.assertCountEqual(expected, actual)

    def testGetScripts3(self):
        yamlStr = '\n'.join([
            "{",
            "  example: {",
            "    test: /^src//,",
            "    script: stack test,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)
        config = convert(configRaw)
        files = ['base/src/path.hs']

        expected = []
        actual = getScripts(config, files)
        self.assertCountEqual(expected, actual)

    def testGetScripts4(self):
        yamlStr = '\n'.join([
            "{",
            "  example: {",
            "    test: /^src//,",
            "    script: stack test,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)
        config = convert(configRaw)
        files = ['src/path.hs']

        name = config[0]['name']
        expected = [notice(name) + 'stack test']
        actual = getScripts(config, files)
        self.assertCountEqual(expected, actual)

    def testGetScripts5(self):
        yamlStr = '\n'.join([
            "{",
            "  example: {",
            "    test: /src$/,",
            "    script: stack test,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)
        config = convert(configRaw)
        files = ['base/src/path.hs']

        expected = []
        actual = getScripts(config, files)
        self.assertCountEqual(expected, actual)

    def testGetScripts6(self):
        yamlStr = '\n'.join([
            "{",
            "  example1: {",
            "    test: /src1//,",
            "    script: stack test 1,",
            "  },",
            "  example2: {",
            "    test: /src2//,",
            "    script: stack test 2,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)
        config = convert(configRaw)
        files = [
            'base/src1/path.hs',
            'base/src2/path.hs'
        ]

        name1 = config[0]['name']
        name2 = config[1]['name']
        expected = [
            notice(name1) + 'stack test 1',
            notice(name2) + 'stack test 2'
        ]
        actual = getScripts(config, files)
        self.assertCountEqual(expected, actual)

    def testGetScripts7(self):
        yamlStr = '\n'.join([
            "{",
            "  example1: {",
            "    test: //src1//,",
            "    script: stack test 1,",
            "  },",
            "  example2: {",
            "    test: //src2//,",
            "    script: stack test 2,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)
        config = convert(configRaw)
        files = [
            'base/src1/path1.hs',
            'base/src1/path2.hs'
        ]

        name1 = config[0]['name']
        expected = [
            notice(name1) + 'stack test 1'
        ]
        actual = getScripts(config, files)
        self.assertCountEqual(expected, actual)

class TestGetAllScripts(unittest.TestCase):
    def testGetAllScripts1(self):
        yamlStr = '\n'.join([
            "{",
            "  example1: {",
            "    test: //src1//,",
            "    script: stack test 1,",
            "  },",
            "  example2: {",
            "    test: //src2//,",
            "    script: stack test 2,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)
        config = convert(configRaw)

        name1 = config[0]['name']
        name2 = config[1]['name']
        expected = [
            notice(name1) + 'stack test 1',
            notice(name2) + 'stack test 2'
        ]
        actual = getAllScripts(config)
        self.assertCountEqual(expected, actual)

    def testGetAllScripts2(self):
        yamlStr = '\n'.join([
            "{",
            "  example1: {",
            "    test: //src1//,",
            "    script: stack test 1,",
            "  },",
            "  example2: {",
            "    test: //src2//,",
            "    script: stack test 1,",
            "  }",
            "}"
        ])
        configRaw = yaml.load(yamlStr)
        config = convert(configRaw)

        name1 = config[0]['name']
        name2 = config[1]['name']
        names = sorted([name1, name2])
        notices = ''.join(map(notice, names))
        expected = [
            notices + 'stack test 1'
        ]
        actual = getAllScripts(config)
        self.assertCountEqual(expected, actual)

if __name__ == "__main__":
    unittest.main()
