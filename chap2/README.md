# Lexer

`tiger.lex` is the good stuff here.

**Great notes here from Prof Edwards**:
https://www.cs.columbia.edu/~sedwards/classes/2002/w4115/tiger.pdf

## Run
```
$ sml
$ CM.make "sources.cm";
$ Parse.parse <test file>;
```

## Checklist:
- Keywords, Symbols
- Identifiers
- Integers (no negatives at the lexer stage)
- Whitespace (spaces, tabs, newlines, returns, and formfeeds)
- Strings
    - "The string value that you return for a string literal should have all the escape sequences translated into their meanings"
    - Escape sequences in strings (quote, backslash, octal digits, Control sequences)
        - After some iterating, I've come to the conclusion that it's easier to have escape sequences be a distinct start state
        - Holy cow, the "use a single slash to let strings span multiple lines" took me forever to understand.
- Comments
    - Nested
    - No EOF in comments, so no option to leave a comment open to the end of the program
        - I think this is reasonable since we support nested comments. If we allowed a hanging "left" comment, it could be tricky to identify comment nesting 


## Notes
- Whitespace are: spaces, tabs, newlines, returns, and formfeeds
- Nested comments! This is simply tracked with a variable representing the "depth" of COMMENT we're in, and kicks us back to INITIAL when we've reached depth of zero.
- I believe we want EOFs in both COMMENT and STRING start states to throw an error. That makes sense, right? Unclosed comments and unclosed strings are errors? The instructions say:
    > Detect unclosed comments (at end of file) and unclosed strings.
    - Nothing happens in the COMMENT start state except that we continue until we find the the comment close operator, "`)*`". The exception is an EOF.
- I tried really hard to maintain good line numbers for error reporting and EOF. 



