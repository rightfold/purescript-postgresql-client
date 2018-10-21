#!/bin/sh

litps compile --file README.md; mv README.purs test/Example.purs; pulp test --main Test.Main

