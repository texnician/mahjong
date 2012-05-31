grammar expr;

options {
    output=AST;
    ASTLabelType=CommonTree;
}

tokens {
    PLUS = '+';
    MINUS = '-';
    MULT = '*';
    DIV = '/';
}

@header {package mahjong;}
@lexer::header {
            package mahjong;
        }

expr: term ((PLUS | MINUS)^ term)*;
term: factor ((MULT | DIV)^ factor)*;
factor: INT ;

INT : '0'..'9'+ ;
WS : ( ' ' | '\t' | 'r' | '\n')+ {$channel=HIDDEN;} ;
        
