purescript-markup-incdom
====================
[![Build Status](https://travis-ci.org/zrho/purescript-markup-incdom.svg?branch=master)](https://travis-ci.org/zrho/purescript-markup)
[![Maintainer: zrho](https://img.shields.io/badge/maintainer-zrho-lightgrey.svg)](http://github.com/zrho)

Incremental DOM rendering backend for `Markup` trees.

Use the `renderTo` function from the `Web.Markup.IncDOM` module to render a
markup tree once to a given DOM node. The markup tree can raise effects in the
`Eff` monad as soon as an event has been triggered; use the `renderTo` function
in event handlers again to achieve an interactive application. When `renderTo`
is applied to a DOM node more than one time, the incremental-dom library will
try its best to keep the update to the DOM minimal.

### Installing

    bower i purescript-markup-incdom

### Building

    bower update
    pulp build
