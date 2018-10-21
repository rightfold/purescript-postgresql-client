#!/bin/sh

litps compile --file README.md; mv README.purs test/Guide.purs; pulp test --main Test.Main

