ace.define("ace/mode/lustre_highlight_rules",["require","exports","module","ace/lib/oop","ace/mode/text_highlight_rules"], function(require, exports, module) {
    "use strict";

    var oop = require("../lib/oop");
    var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

    var LustreHighlightRules = function() {
        var keywords = "open|type|const|fun|node|returns|var|let|tel" +
            "|if|then|else|fby|when|merge|switch|do|end|reset|every|" +
            "automaton|state|until|unless|continue|last|" +
            "and|or|xor|not|" + // operators
            "class|instance|step|case|of|in|default|skip|" + // obc coloring
            "break|while|return"; // C coloring

        var builtinConstants =
            "true|false";

        var storageType =
            "int|int32|int64|uint|uint32|uint64|bool|real|on|" +
            "void|char|unsigned|struct|register"; // C types

        var keywordMapper = this.createKeywordMapper({
            "keyword": keywords,
            "constant.language": builtinConstants,
            "storage.type": storageType,
        }, "identifier", true);

        this.$rules = {
            "start" : [ {
                token : "comment",
                regex : "--.*$"
            }, {
                token : "string",           // " string
                regex : '".*?"'
            }, {
                token : "string",           // character
                regex : "'.'"
            }, {
                token : "constant.numeric", // float
                regex : "[+-]?\\d+(?:(?:\\.\\d*)?(?:[eE][+-]?\\d+)?)?\\b"
            }, {
                token : keywordMapper,
                regex : "[a-zA-Z_$][a-zA-Z0-9_$]*\\b"
            }, {
                token : "keyword.operator",
                regex : "\\+|\\-|\\/|<|>|<=|=>|="
            }, {
                token : "paren.lparen",
                regex : "[\\(]"
            }, {
                token : "paren.rparen",
                regex : "[\\)]"
            }, {
                token : "text",
                regex : "\\s+"
            } ]
        };
    };

    oop.inherits(LustreHighlightRules, TextHighlightRules);

    exports.LustreHighlightRules = LustreHighlightRules;
});

ace.define("ace/mode/lustre",["require","exports","module","ace/lib/oop","ace/mode/text","ace/mode/lustre_highlight_rules","ace/range"], function(require, exports, module) {
    "use strict";

    var oop = require("../lib/oop");
    var TextMode = require("./text").Mode;
    var LustreHighlightRules = require("./lustre_highlight_rules").LustreHighlightRules;
    var Range = require("../range").Range;

    var Mode = function() {
        this.HighlightRules = LustreHighlightRules;
        this.$behaviour = this.$defaultBehaviour;
    };
    oop.inherits(Mode, TextMode);

    (function() {

        this.lineCommentStart = "--";

        this.getNextLineIndent = function(state, line, tab) {
            var indent = this.$getIndent(line);

            var tokenizedLine = this.getTokenizer().getLineTokens(line, state);
            var tokens = tokenizedLine.tokens;

            if (tokens.length && tokens[tokens.length-1].type == "comment") {
                return indent;
            }
            if (state == "start") {
                if (line.match(/^.*(let)\s*$/)) {
                    indent += tab;
                }
            }

            return indent;
        };

        this.checkOutdent = function(state, line, input) {
            var complete_line = line + input;
            if (complete_line.match(/^\s*(tel)$/)) {
                return true;
            }

            return false;
        };

        this.autoOutdent = function(state, session, row) {

            var line = session.getLine(row);
            var prevLine = session.getLine(row - 1);
            var prevIndent = this.$getIndent(prevLine).length;
            var indent = this.$getIndent(line).length;

            session.outdentRows(new Range(row, 0, row + 2, 0));
        };


        this.$id = "ace/mode/lustre";
    }).call(Mode.prototype);

    exports.Mode = Mode;

});                (function() {
    ace.require(["ace/mode/lustre"], function(m) {
        if (typeof module == "object" && typeof exports == "object" && module) {
            module.exports = m;
        }
    });
})();
