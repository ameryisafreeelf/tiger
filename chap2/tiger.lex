type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val stringBuf : string ref = ref ""
val inString : bool ref = ref false
val stringBegin: int ref = ref 0
val commentDepth: int ref = ref 0;

fun decimalEscapeToDigit str =
    let val justInt = valOf(Int.fromString(str))
        val intToChar = chr justInt
        val charToString = Char.toString intToChar
    in charToString end

fun eof() = 
    let 
        val pos = hd(!linePos) 
    in 
        if (!commentDepth <> 0) then ErrorMsg.error pos ("EOF in comment")
        else if (!inString <> false) then ErrorMsg.error pos ("EOF in string")
        else ();
        Tokens.EOF(pos, pos)
end

%%

%s COMMENT STRING ESCAPE;

digit = [0-9]+;
identifier = [a-zA-Z][a-zA-Z0-9_]*;

%%
  
<INITIAL>"type"	            => (linePos := (yypos + 4) :: !linePos;
                                Tokens.TYPE(yypos, yypos + 4));
<INITIAL>"var"              => (linePos := (yypos + 3) :: !linePos;
                                Tokens.VAR(yypos, yypos + 3));
<INITIAL>"function"         => (linePos := (yypos + 8) :: !linePos;
                                Tokens.FUNCTION(yypos, yypos + 8));
<INITIAL>"break"            => (linePos := (yypos + 5) :: !linePos;
                                Tokens.BREAK(yypos, yypos + 5));
<INITIAL>"of"               => (linePos := (yypos + 2) :: !linePos;
                                Tokens.OF(yypos, yypos + 2));
<INITIAL>"end"              => (linePos := (yypos + 3) :: !linePos;
                                Tokens.END(yypos, yypos + 3));
<INITIAL>"in"               => (linePos := (yypos + 2) :: !linePos;
                                Tokens.IN(yypos, yypos + 2));
<INITIAL>"nil"              => (linePos := (yypos + 3) :: !linePos;
                                Tokens.NIL(yypos, yypos + 3));
<INITIAL>"let"              => (linePos := (yypos + 3) :: !linePos;
                                Tokens.LET(yypos, yypos + 3));
<INITIAL>"do"               => (linePos := (yypos + 2) :: !linePos;
                                Tokens.DO(yypos, yypos + 2));
<INITIAL>"to"               => (linePos := (yypos + 2) :: !linePos;
                                Tokens.TO(yypos, yypos + 2));
<INITIAL>"for"              => (linePos := (yypos + 3) :: !linePos;
                                Tokens.FOR(yypos, yypos + 3));
<INITIAL>"while"            => (linePos := (yypos + 5) :: !linePos;
                                Tokens.WHILE(yypos, yypos + 5));
<INITIAL>"else"             => (linePos := (yypos + 4) :: !linePos;
                                Tokens.ELSE(yypos, yypos + 4));
<INITIAL>"then"             => (linePos := (yypos + 4) :: !linePos;
                                Tokens.THEN(yypos, yypos + 4));
<INITIAL>"if"               => (linePos := (yypos + 2) :: !linePos;
                                Tokens.IF(yypos, yypos + 2));
<INITIAL>"array"            => (linePos := (yypos + 5) :: !linePos;
                                Tokens.ARRAY(yypos, yypos + 5));

<INITIAL>":="               => (linePos := (yypos + 2) :: !linePos;
                                Tokens.ASSIGN(yypos, yypos + 2));
<INITIAL>"|"                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.OR(yypos, yypos + 1));
<INITIAL>"&"                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.AND(yypos, yypos + 1));
<INITIAL>">="               => (linePos := (yypos + 2) :: !linePos;
                                Tokens.GE(yypos, yypos + 2));
<INITIAL>">"                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.GT(yypos, yypos + 1));
<INITIAL>"<="               => (linePos := (yypos + 2) :: !linePos;
                                Tokens.LE(yypos, yypos + 2));
<INITIAL>"<"                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.LT(yypos, yypos + 1));
<INITIAL>"<>"               => (linePos := (yypos + 2) :: !linePos;
                                Tokens.NEQ(yypos, yypos + 2));
<INITIAL>"="                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.EQ(yypos, yypos + 1));
<INITIAL>"/"                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.DIVIDE(yypos, yypos + 1));
<INITIAL>"*"                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.TIMES(yypos, yypos + 1));
<INITIAL>"+"                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.PLUS(yypos, yypos + 1));
<INITIAL>"-"                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.MINUS(yypos, yypos + 1));
<INITIAL>"."                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.DOT(yypos, yypos + 1));
<INITIAL>"{"                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.LBRACE(yypos, yypos + 1));
<INITIAL>"}"                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.RBRACE(yypos, yypos + 1));
<INITIAL>"["                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.LBRACK(yypos, yypos + 1));
<INITIAL>"]"                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.RBRACK(yypos, yypos + 1));
<INITIAL>"("                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.LPAREN(yypos, yypos + 1));
<INITIAL>")"                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.RPAREN(yypos, yypos + 1));
<INITIAL>";"                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.SEMICOLON(yypos, yypos + 1));
<INITIAL>":"                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.COLON(yypos, yypos + 1));
<INITIAL>","                => (linePos := (yypos + 1) :: !linePos;
                                Tokens.COMMA(yypos, yypos + 1));

<INITIAL>[\ \t\r\f]*        => (linePos := (yypos + 1) :: !linePos;
                                continue());
<INITIAL>{digit}            => (linePos := (yypos + size(yytext)) :: !linePos;
                                Tokens.INT(valOf(Int.fromString yytext), yypos, yypos + size yytext));
<INITIAL>{identifier}       => (linePos := (yypos + size(yytext)) :: !linePos;
                                Tokens.ID(yytext, yypos, yypos + size yytext));

<INITIAL>"\""               => (linePos := (yypos + 1) :: !linePos;
                                YYBEGIN STRING;
                                stringBegin := yypos;
                                stringBuf := "";
                                inString := true;
                                continue());

<STRING>\\                  => (YYBEGIN ESCAPE;
                                continue());

<STRING>\\[\ \n\t\r\f]*\\   => (linePos := (yypos + size(yytext)) :: !linePos;
                                lineNum := (!lineNum) + 1; 
                                linePos := yypos :: !linePos; 
                                continue());

<ESCAPE>n                   => (linePos := (yypos + 1) :: !linePos;
                                lineNum := (!lineNum) + 1;
                                stringBuf := !stringBuf ^ "\n";
                                YYBEGIN STRING;
                                continue());

<ESCAPE>t			        => (linePos := (yypos + 1) :: !linePos;
                                stringBuf := !stringBuf ^ "\t";
                                YYBEGIN STRING;
							    continue());

<ESCAPE>^[A-Z@\[\\\]^_]     => (linePos := (yypos + 1) :: !linePos;
                                stringBuf := !stringBuf ^ yytext;
                                YYBEGIN STRING;
                                continue());

<ESCAPE>\\                  => (linePos := (yypos + 1) :: !linePos;
                                stringBuf := !stringBuf ^ "\\";
                                YYBEGIN STRING;
                                continue());

<ESCAPE>\"                  => (linePos := (yypos + 1) :: !linePos;
                                stringBuf := !stringBuf ^ "\"";
                                YYBEGIN STRING;
                                continue());

<ESCAPE>[0-9]{3}            => (linePos := (yypos + 1) :: !linePos;
                                stringBuf := !stringBuf ^ decimalEscapeToDigit(yytext);
                                YYBEGIN STRING;
                                continue());

<ESCAPE>.                   => (ErrorMsg.error yypos ("illegal escape character " ^ yytext); 
                                continue());

<STRING>\n                  => (linePos := (yypos + 1) :: !linePos;
                                lineNum := (!lineNum) + 1; 
                                stringBuf := !stringBuf ^ "\n";
                                continue());

<STRING>[\t\r\f]*           => (linePos := (yypos + 1) :: !linePos;
                                stringBuf := !stringBuf ^ yytext;
                                continue());

<STRING>"\""                => (linePos := (yypos + 1) :: !linePos;
                                YYBEGIN INITIAL; 
                                inString := false;
                                Tokens.STRING(!stringBuf, !stringBegin, yypos));

<STRING>.                   => (linePos := (yypos + size(yytext)) :: !linePos;
                                stringBuf := !stringBuf ^ yytext; 
                                continue());

<INITIAL>"/*"               => (linePos := (yypos + 1) :: !linePos;
                                YYBEGIN COMMENT;
                                commentDepth := !commentDepth + 1;
                                continue());

<COMMENT>"/*"               =>  (linePos := (yypos + 1) :: !linePos;
                                commentDepth := !commentDepth + 1;
                                continue());

<COMMENT>"*/"               => (linePos := (yypos + 1) :: !linePos;
                                commentDepth := !commentDepth - 1;
                                if (!commentDepth = 0) then YYBEGIN INITIAL else (); 
                                continue());   

<COMMENT>.                  => (linePos := (yypos + size(yytext)) :: !linePos;
                                continue());

<INITIAL, COMMENT>"\n"      => (lineNum := (!lineNum) + 1; 
                                linePos := yypos :: !linePos; 
                                continue());
                                
.                           => (ErrorMsg.error yypos ("illegal character " ^ yytext); 
                                continue());
