grammar dl;

options {
    output=AST;
    ASTLabelType=CommonTree;
}

@header { package mahjong; }
@lexer::header {
            package mahjong;
        }

// // 234#345678@4468$
tile_seq:
        (chi_pong|gang)*free_tiles
    ;

chi_pong:
        chi_pong_tiles CHIMARK^
    ;

chi_pong_tiles:
        ENUM ENUM ENUM (WAN|BING|TIAO|FENG|JIAN)^
    ;

gang:
        gang_tiles (GANG|CHIMARK)^
    ;

gang_tiles:
        ENUM ENUM ENUM ENUM (WAN|BING|TIAO|FENG|JIAN)^
    ;

free_tiles:
        tile_group tile_group*
    ;

tile_group:
        ENUM+ (WAN|BING|TIAO|FENG|JIAN)^
    ;

WAN : ('W'|'w') ;
BING : ('B'|'b') ;
TIAO : ('T'|'t') ;
FENG : ('F'|'f') ;
JIAN : ('J'|'j') ;
GANG : '-' ;
CHIMARK : '^' ;
ENUM : '1'..'9' ;
WS : ( ' ' | '\t' | '\r' | '\n')+ {$channel=HIDDEN;} ;
