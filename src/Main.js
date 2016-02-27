"use strict";

// module Main

exports.scrollBottomImpl = function(elemId) {
    return function() {
        var elem = document.getElementById(elemId);
        if (elem) {
            elem.scrollTop = 1e100;
        }
    };
};

