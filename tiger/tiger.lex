structure T = Tokens

type lineNo            = int
type pos               = lineNo  
val  lineRef : pos ref = ref 1; 
val  colRef  : int ref = ref 0;
val  eolpos  : int ref = ref 0;
val comment_level      = ref 0;
val  temp_string       = ref "";
val inside_string      = ref 0;  
(* (temp_string := !temp_string ^ asciiCode(yytext); continue()); *)
type svalue        = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult     = (svalue,pos) token

fun asciiCode str =
    let val subStr = String.substring(str, 1, 3)
        val intVal = valOf(Int.fromString(subStr))
        val charVal = chr intVal
    in Char.toString charVal end

fun updateLine n   = lineRef := !(lineRef) + n

fun lineRange l r  = "line [" ^ Int.toString l ^ " - " ^ Int.toString r ^ "]"

fun error (e,l,r)  = TextIO.output(TextIO.stdErr, lineRange l r ^ " : " ^ e ^ "\n")

fun eof   ()       = if !comment_level <> 0 then (error("Comments are not closed", !lineRef, !colRef); lineRef := 1; colRef:= 1; T.EOF (!lineRef,!colRef))
                     else if !inside_string = 1 then (error("String expression not closed", !lineRef, !colRef); lineRef := 1; colRef:= 1; T.EOF (!lineRef,!colRef))
                     else (lineRef := 1; colRef:= 1; T.EOF (!lineRef,!colRef))



val newlineCount = List.length o List.filter (fn x => x = #"\n") o String.explode

%%

%header (functor TigerLexFun(structure Tokens : Tiger_TOKENS));
%s STRING COMMENT;
string_regexp = [a-zA-Z][a-zA-Z0-9_];
digit = [0-9];

%%

<INITIAL>"array"                   => (colRef := yypos-(!eolpos); T.ARRAY(!lineRef, !colRef));
<INITIAL>"if"                      => (colRef := yypos-(!eolpos); T.IF(!lineRef, !colRef));
<INITIAL>"then"                    => (colRef := yypos-(!eolpos); T.THEN(!lineRef, !colRef));
<INITIAL>"else"                    => (colRef := yypos-(!eolpos); T.ELSE(!lineRef, !colRef));
<INITIAL>"while"                   => (colRef := yypos-(!eolpos); T.WHILE(!lineRef, !colRef));
<INITIAL>"for"                     => (colRef := yypos-(!eolpos); T.FOR(!lineRef, !colRef));
<INITIAL>"to"                      => (colRef := yypos-(!eolpos); T.TO(!lineRef, !colRef));
<INITIAL>"do"                      => (colRef := yypos-(!eolpos); T.DO(!lineRef, !colRef));
<INITIAL>"let"                     => (colRef := yypos-(!eolpos); T.LET(!lineRef, !colRef));
<INITIAL>"in"                      => (colRef := yypos-(!eolpos); T.IN(!lineRef, !colRef));
<INITIAL>"end"                     => (colRef := yypos-(!eolpos); T.END(!lineRef, !colRef));
<INITIAL>"of"                      => (colRef := yypos-(!eolpos); T.OF(!lineRef, !colRef));
<INITIAL>"break"                   => (colRef := yypos-(!eolpos); T.BREAK(!lineRef, !colRef));
<INITIAL>"nil"                     => (colRef := yypos-(!eolpos); T.NIL(!lineRef, !colRef));
<INITIAL>"function"                => (colRef := yypos-(!eolpos); T.FUNCTION(!lineRef, !colRef));
<INITIAL>"var"                     => (colRef := yypos-(!eolpos); T.VAR(!lineRef, !colRef));
<INITIAL>"type"                    => (colRef := yypos-(!eolpos); T.TYPE(!lineRef, !colRef));
<INITIAL>"primitive"               => (colRef := yypos-(!eolpos); T.PRIMITIVE(!lineRef, !colRef));
<INITIAL>"class"                   => (colRef := yypos-(!eolpos); T.CLASS(!lineRef, !colRef));
<INITIAL>"extends"                 => (colRef := yypos-(!eolpos); T.EXTENDS(!lineRef, !colRef));
<INITIAL>"method"                  => (colRef := yypos-(!eolpos); T.METHOD(!lineRef, !colRef));
<INITIAL>"new"                     => (colRef := yypos-(!eolpos); T.NEW(!lineRef, !colRef));

<INITIAL>"("                       => (colRef := yypos-(!eolpos); T.LEFT_PAREN(!lineRef, !colRef));
<INITIAL>")"                       => (colRef := yypos-(!eolpos); T.RIGHT_PAREN(!lineRef, !colRef));
<INITIAL>"{"                       => (colRef := yypos-(!eolpos); T.LEFT_CURLY(!lineRef, !colRef));
<INITIAL>"}"                       => (colRef := yypos-(!eolpos); T.RIGHT_CURLY(!lineRef, !colRef));
<INITIAL>"["                       => (colRef := yypos-(!eolpos); T.LEFT_BRACKET(!lineRef, !colRef));
<INITIAL>"]"                       => (colRef := yypos-(!eolpos); T.RIGHT_BRACKET(!lineRef, !colRef));
<INITIAL>","                       => (colRef := yypos-(!eolpos); T.COMMA(!lineRef, !colRef));
<INITIAL>":"                       => (colRef := yypos-(!eolpos); T.COLON(!lineRef, !colRef));
<INITIAL>";"                       => (colRef := yypos-(!eolpos); T.SEMICOLON(!lineRef, !colRef));
<INITIAL>":="                      => (colRef := yypos-(!eolpos); T.ASSIGN(!lineRef, !colRef));
<INITIAL>"."                       => (colRef := yypos-(!eolpos); T.DOT(!lineRef, !colRef));
<INITIAL>"*"                       => (colRef := yypos-(!eolpos); T.MUL(!lineRef, !colRef));
<INITIAL>"+"                       => (colRef := yypos-(!eolpos); T.PLUS(!lineRef, !colRef));
<INITIAL>"-"                       => (colRef := yypos-(!eolpos); T.MINUS(!lineRef, !colRef));
<INITIAL>"/"                       => (colRef := yypos-(!eolpos); T.DIV(!lineRef, !colRef));
<INITIAL>"="                       => (colRef := yypos-(!eolpos); T.EQUAL(!lineRef, !colRef));
<INITIAL>"<>"                      => (colRef := yypos-(!eolpos); T.NOT_EQUAL(!lineRef, !colRef));
<INITIAL>">"                       => (colRef := yypos-(!eolpos); T.GT(!lineRef, !colRef));
<INITIAL>">="                      => (colRef := yypos-(!eolpos); T.GTE(!lineRef, !colRef));
<INITIAL>"<"                       => (colRef := yypos-(!eolpos); T.LT(!lineRef, !colRef));
<INITIAL>"<="                      => (colRef := yypos-(!eolpos); T.LTE(!lineRef, !colRef));
<INITIAL>"|"                       => (colRef := yypos-(!eolpos); T.OR(!lineRef, !colRef));
<INITIAL>"&"                       => (colRef := yypos-(!eolpos); T.AND(!lineRef, !colRef));


<INITIAL>[a-zA-Z][a-zA-Z0-9_]*	       => (colRef := yypos-(!eolpos); T.ID(yytext, !lineRef, !colRef));


<INITIAL>{digit}+ 	               => (colRef := yypos-(!eolpos); T.INTEXP(valOf(Int.fromString yytext), !lineRef, !colRef));

<INITIAL>\n              	       => (updateLine 1; eolpos:=yypos + size yytext; continue());
<INITIAL>[\ \t]+                   => (continue());


<INITIAL>"/*"                      => (YYBEGIN COMMENT; comment_level := 1; continue());
<INITIAL>"*/"                      => (colRef := yypos-(!eolpos); error("Can't close unopened comment", !lineRef, !colRef); continue());
<COMMENT>"*/"                      => (comment_level := !comment_level-1; 
                                        if !comment_level = 0 then (YYBEGIN INITIAL; continue())
                                        else continue());
<COMMENT>"/*"                      => (comment_level := !comment_level+1; continue());
<COMMENT>\n                        => (updateLine 1; eolpos:=yypos + size yytext; continue());
<COMMENT>[\ \t]+                   => (continue());
<COMMENT>.                         => (continue());


<INITIAL>\"                        => (YYBEGIN STRING; colRef := yypos-(!eolpos); inside_string := 1; !temp_string = ""; continue());
<STRING>\n                         => (error("String should be written in one line", !lineRef, !colRef); continue());
<STRING>\\n                        => (temp_string := !temp_string ^ "\n"; continue());
<STRING>\\a                        => (temp_string := !temp_string ^ "\a"; continue());
<STRING>\\b                        => (temp_string := !temp_string ^ "\b"; continue());
<STRING>\\f                        => (temp_string := !temp_string ^ "\f"; continue());
<STRING>\\r                        => (temp_string := !temp_string ^ "\r"; continue());
<STRING>\\v                        => (temp_string := !temp_string ^ "\v"; continue());
<STRING>\\t                        => (temp_string := !temp_string ^ "\t"; continue());
<STRING>\\\\                       => (temp_string := !temp_string ^ "\\"; continue());
<STRING>\\\"                       => (temp_string := !temp_string ^ "\""; continue());
<STRING>[^\\"\n]                   => (temp_string := !temp_string ^ yytext; continue());
<STRING>\"                         => (YYBEGIN INITIAL; inside_string := 0; T.STRINGEXP (!temp_string, !lineRef, !colRef));
<STRING>\\[ \t\n\f]+\\	           => (continue());
<STRING>\\[0-9][0-9][0-9]          => (let val x = valOf(Int.fromString(String.substring(yytext, 1, 3)))
                                       in if x > 255 then (error("ASCII Code can't be greater than 255.", !lineRef, !colRef); continue())
                                          else (temp_string := !temp_string ^ (Char.toString (Char.chr x));continue()) 
                                       end  
                                       );
                                       
                                       
<STRING>.                          => (error("Unexpected/Illegal character", !lineRef, !colRef); continue());


<INITIAL>.                         => (error("Unexpected/Illegal character", !lineRef, !colRef); continue());









