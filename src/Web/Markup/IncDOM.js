/* global exports, require */
"use strict";

// module Web.Markup.IncDOM

var incDom = require('incremental-dom');

exports._elementOpenStart = incDom.elementOpenStart;
exports._elementOpenEnd = incDom.elementOpenEnd;
exports._elementClose = incDom.elementClose;
exports._attr = incDom.attr;
exports._text = incDom.text;
exports._handler = incDom.attr;
exports._patch = incDom.patch;
