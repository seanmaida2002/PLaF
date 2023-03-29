
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
  type token = 
    | UNPAIR
    | UNITTYPE
    | TL
    | TIMES
    | THEN
    | SUPER
    | SUM
    | SND
    | SETREF
    | SET
    | SEND
    | SEMICOLON
    | SELF
    | RRANGLE
    | RPAREN
    | REFTYPE
    | RBRACE
    | RANGLE
    | PROD
    | PROC
    | PLUS
    | PAIR
    | OF
    | NODE
    | NEWREF
    | NEW
    | MINUS
    | MIN
    | METHOD
    | MAXL
    | LPAREN
    | LLANGLE
    | LIST
    | LETREC
    | LET
    | LBRACE
    | LANGLE
    | ISZERO
    | ISNUMBER
    | INTTYPE
    | INTERFACE
    | INT of (
# 15 "lib/grammar.mly"
       (int)
# 55 "lib/grammar.ml"
  )
    | INSTANCEOF
    | IN
    | IMPLEMENTS
    | IF
    | ID of (
# 16 "lib/grammar.mly"
       (string)
# 64 "lib/grammar.ml"
  )
    | HD
    | FST
    | FIELD
    | EXTENDS
    | EQUALSMUTABLE
    | EQUALS
    | EOF
    | END
    | EMPTYTREE
    | EMPTYPRED
    | EMPTYLIST
    | ELSE
    | DOT
    | DIVIDED
    | DEREF
    | DEBUG
    | CONS
    | COMMA
    | COLON
    | CLASS
    | CAST
    | CASET
    | BOOLTYPE
    | BEGIN
    | AVG
    | ARROW
    | ABS
  
end

include MenhirBasics

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState349
  | MenhirState347
  | MenhirState346
  | MenhirState340
  | MenhirState338
  | MenhirState337
  | MenhirState334
  | MenhirState330
  | MenhirState329
  | MenhirState328
  | MenhirState327
  | MenhirState325
  | MenhirState319
  | MenhirState317
  | MenhirState316
  | MenhirState315
  | MenhirState314
  | MenhirState311
  | MenhirState309
  | MenhirState305
  | MenhirState303
  | MenhirState302
  | MenhirState301
  | MenhirState299
  | MenhirState298
  | MenhirState297
  | MenhirState296
  | MenhirState295
  | MenhirState293
  | MenhirState289
  | MenhirState288
  | MenhirState287
  | MenhirState283
  | MenhirState281
  | MenhirState280
  | MenhirState278
  | MenhirState276
  | MenhirState273
  | MenhirState272
  | MenhirState269
  | MenhirState268
  | MenhirState267
  | MenhirState266
  | MenhirState265
  | MenhirState264
  | MenhirState262
  | MenhirState261
  | MenhirState260
  | MenhirState258
  | MenhirState253
  | MenhirState252
  | MenhirState251
  | MenhirState247
  | MenhirState245
  | MenhirState241
  | MenhirState240
  | MenhirState239
  | MenhirState238
  | MenhirState237
  | MenhirState236
  | MenhirState234
  | MenhirState232
  | MenhirState230
  | MenhirState228
  | MenhirState226
  | MenhirState224
  | MenhirState223
  | MenhirState222
  | MenhirState218
  | MenhirState216
  | MenhirState215
  | MenhirState205
  | MenhirState204
  | MenhirState200
  | MenhirState198
  | MenhirState197
  | MenhirState192
  | MenhirState191
  | MenhirState187
  | MenhirState186
  | MenhirState184
  | MenhirState183
  | MenhirState182
  | MenhirState181
  | MenhirState180
  | MenhirState179
  | MenhirState178
  | MenhirState177
  | MenhirState176
  | MenhirState175
  | MenhirState174
  | MenhirState173
  | MenhirState170
  | MenhirState169
  | MenhirState168
  | MenhirState167
  | MenhirState165
  | MenhirState163
  | MenhirState162
  | MenhirState161
  | MenhirState159
  | MenhirState157
  | MenhirState155
  | MenhirState152
  | MenhirState149
  | MenhirState147
  | MenhirState144
  | MenhirState143
  | MenhirState140
  | MenhirState138
  | MenhirState136
  | MenhirState135
  | MenhirState133
  | MenhirState132
  | MenhirState127
  | MenhirState125
  | MenhirState123
  | MenhirState121
  | MenhirState119
  | MenhirState116
  | MenhirState115
  | MenhirState113
  | MenhirState111
  | MenhirState110
  | MenhirState108
  | MenhirState106
  | MenhirState103
  | MenhirState101
  | MenhirState99
  | MenhirState97
  | MenhirState94
  | MenhirState91
  | MenhirState88
  | MenhirState87
  | MenhirState84
  | MenhirState82
  | MenhirState80
  | MenhirState78
  | MenhirState75
  | MenhirState73
  | MenhirState66
  | MenhirState63
  | MenhirState61
  | MenhirState60
  | MenhirState58
  | MenhirState54
  | MenhirState53
  | MenhirState44
  | MenhirState40
  | MenhirState32
  | MenhirState31
  | MenhirState30
  | MenhirState23
  | MenhirState17
  | MenhirState15
  | MenhirState10
  | MenhirState8
  | MenhirState7
  | MenhirState6
  | MenhirState4
  | MenhirState3
  | MenhirState0

# 9 "lib/grammar.mly"
  
open Ast

# 273 "lib/grammar.ml"

let rec _menhir_goto_separated_nonempty_list_SEMICOLON_field_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * (bool * Ast.expr)) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((string * (bool * Ast.expr)) list)) = _v in
        let _v : ((string * (bool * Ast.expr)) list) = 
# 144 "<standard.mly>"
    ( x )
# 285 "lib/grammar.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_field__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState258 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((string * (bool * Ast.expr)) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string * (bool * Ast.expr)))) = _menhir_stack in
        let _v : ((string * (bool * Ast.expr)) list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 296 "lib/grammar.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_field_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_formal_par_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.texpr option) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState334 | MenhirState63 | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((string * Ast.texpr option) list)) = _v in
        let _v : ((string * Ast.texpr option) list) = 
# 144 "<standard.mly>"
    ( x )
# 312 "lib/grammar.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_formal_par__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((string * Ast.texpr option) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string * Ast.texpr option))) = _menhir_stack in
        let _v : ((string * Ast.texpr option) list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 323 "lib/grammar.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_formal_par_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_formal_par__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.texpr option) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (retType : (Ast.texpr))), (id : (
# 16 "lib/grammar.mly"
       (string)
# 345 "lib/grammar.ml"
            ))), _, (xs : ((string * Ast.texpr option) list))) = _menhir_stack in
            let _v : (Ast.abs_mdecl) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 350 "lib/grammar.ml"
             in
            
# 228 "lib/grammar.mly"
         ( MethodAbs(id,retType,params) )
# 355 "lib/grammar.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | METHOD ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | RBRACE ->
                _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ABS ->
                    _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | AVG ->
                    _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | BEGIN ->
                    _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | CASET ->
                    _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | CAST ->
                    _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | CONS ->
                    _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | DEBUG ->
                    _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | DEREF ->
                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | EMPTYLIST ->
                    _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | EMPTYPRED ->
                    _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | EMPTYTREE ->
                    _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | FST ->
                    _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | HD ->
                    _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | ID _v ->
                    _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
                | IF ->
                    _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | INSTANCEOF ->
                    _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | INT _v ->
                    _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
                | ISNUMBER ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | ISZERO ->
                    _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | LANGLE ->
                    _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | LBRACE ->
                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | LET ->
                    _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | LETREC ->
                    _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | LIST ->
                    _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | LPAREN ->
                    _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | MAXL ->
                    _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | MIN ->
                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | NEW ->
                    _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | NEWREF ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | NODE ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | PAIR ->
                    _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | PROC ->
                    _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | PROD ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | SELF ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | SEND ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | SET ->
                    _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | SETREF ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | SND ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | SUM ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | SUPER ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | TL ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | UNPAIR ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState334 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ABS ->
                    _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | AVG ->
                    _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | BEGIN ->
                    _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | CASET ->
                    _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | CAST ->
                    _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | CONS ->
                    _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | DEBUG ->
                    _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | DEREF ->
                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | EMPTYLIST ->
                    _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | EMPTYPRED ->
                    _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | EMPTYTREE ->
                    _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | FST ->
                    _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | HD ->
                    _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | ID _v ->
                    _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
                | IF ->
                    _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | INSTANCEOF ->
                    _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | INT _v ->
                    _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
                | ISNUMBER ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | ISZERO ->
                    _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | LANGLE ->
                    _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | LBRACE ->
                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | LET ->
                    _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | LETREC ->
                    _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | LIST ->
                    _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | LPAREN ->
                    _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | MAXL ->
                    _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | MIN ->
                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | NEW ->
                    _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | NEWREF ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | NODE ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | PAIR ->
                    _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | PROC ->
                    _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | PROD ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | SELF ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | SEND ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | SET ->
                    _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | SETREF ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | SND ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | SUM ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | SUPER ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | TL ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | UNPAIR ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState337
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState337)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_method_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.mdecl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState340 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.mdecl))), _, (xs : (Ast.mdecl list))) = _menhir_stack in
        let _v : (Ast.mdecl list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 620 "lib/grammar.ml"
         in
        _menhir_goto_list_method_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (id1 : (
# 16 "lib/grammar.mly"
       (string)
# 635 "lib/grammar.ml"
            ))), (id2 : (
# 16 "lib/grammar.mly"
       (string)
# 639 "lib/grammar.ml"
            ))), (id3 : (string option))), _, (ofs : ((string * Ast.texpr option) list))), _, (mths : (Ast.mdecl list))) = _menhir_stack in
            let _v : (Ast.cdecl) = 
# 205 "lib/grammar.mly"
  ( Class(id1,id2,id3,ofs,mths))
# 644 "lib/grammar.ml"
             in
            _menhir_goto_iface_or_class_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (string list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (
# 16 "lib/grammar.mly"
       (string)
# 666 "lib/grammar.ml"
        ))) = _menhir_stack in
        let _v : (string list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 671 "lib/grammar.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (string list)) = _v in
        let _v : (string list) = 
# 144 "<standard.mly>"
    ( x )
# 681 "lib/grammar.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_method_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.mdecl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | METHOD ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState340
    | RBRACE ->
        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState340
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState340

and _menhir_goto_nonempty_list_rdecs_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.rdecs) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState269 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * string * Ast.texpr option * Ast.texpr option * Ast.expr))), _, (xs : (Ast.rdecs))) = _menhir_stack in
        let _v : (Ast.rdecs) = 
# 223 "<standard.mly>"
    ( x :: xs )
# 714 "lib/grammar.ml"
         in
        _menhir_goto_nonempty_list_rdecs_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState272 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState272 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState272
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState272)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_field : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * (bool * Ast.expr)) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState258)
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (string * (bool * Ast.expr)))) = _menhir_stack in
        let _v : ((string * (bool * Ast.expr)) list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 848 "lib/grammar.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_field_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.expr list)) = _v in
        let _v : (Ast.expr list) = 
# 144 "<standard.mly>"
    ( x )
# 868 "lib/grammar.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.expr list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Ast.expr))), _) = _menhir_stack in
        let _v : (Ast.expr list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 879 "lib/grammar.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState78 | MenhirState80 | MenhirState311 | MenhirState91 | MenhirState106 | MenhirState110 | MenhirState115 | MenhirState136 | MenhirState165 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.expr list)) = _v in
        let _v : (Ast.expr list) = 
# 144 "<standard.mly>"
    ( x )
# 895 "lib/grammar.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.expr list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Ast.expr))), _) = _menhir_stack in
        let _v : (Ast.expr list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 906 "lib/grammar.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run169 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | AVG ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | BEGIN ->
        _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | CASET ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | CAST ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | CONS ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | DEBUG ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | DEREF ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | EMPTYLIST ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | EMPTYPRED ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | EMPTYTREE ->
        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | FST ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | HD ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | ID _v ->
        _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
    | IF ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | INSTANCEOF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | INT _v ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v
    | ISNUMBER ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | ISZERO ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | LANGLE ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | LBRACE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | LETREC ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | LIST ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | LPAREN ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | MAXL ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | MIN ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | NEW ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | NEWREF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | NODE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | PAIR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | PROC ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | PROD ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | SELF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | SEND ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | SET ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | SETREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | SND ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | SUM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | SUPER ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | TL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | UNPAIR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState169
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169

and _menhir_run175 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | AVG ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | BEGIN ->
        _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | CASET ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | CAST ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | CONS ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | DEBUG ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | DEREF ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | EMPTYLIST ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | EMPTYPRED ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | EMPTYTREE ->
        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | FST ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | HD ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | ID _v ->
        _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | IF ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | INSTANCEOF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | INT _v ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | ISNUMBER ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | ISZERO ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | LANGLE ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | LBRACE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | LETREC ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | LIST ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | LPAREN ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | MAXL ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | MIN ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | NEW ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | NEWREF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | NODE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | PAIR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | PROC ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | PROD ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | SELF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | SEND ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | SET ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | SETREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | SND ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | SUM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | SUPER ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | TL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | UNPAIR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState175
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175

and _menhir_run179 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | AVG ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | BEGIN ->
        _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | CASET ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | CAST ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | CONS ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | DEBUG ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | DEREF ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | EMPTYLIST ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | EMPTYPRED ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | EMPTYTREE ->
        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | FST ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | HD ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | ID _v ->
        _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | IF ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | INSTANCEOF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | INT _v ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | ISNUMBER ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | ISZERO ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | LANGLE ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | LBRACE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | LETREC ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | LIST ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | LPAREN ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | MAXL ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | MIN ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | NEW ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | NEWREF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | NODE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | PAIR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | PROC ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | PROD ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | SELF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | SEND ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | SET ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | SETREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | SND ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | SUM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | SUPER ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | TL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | UNPAIR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179

and _menhir_run181 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | AVG ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | BEGIN ->
        _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | CASET ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | CAST ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | CONS ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | DEBUG ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | DEREF ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | EMPTYLIST ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | EMPTYPRED ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | EMPTYTREE ->
        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | FST ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | HD ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | ID _v ->
        _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
    | IF ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | INSTANCEOF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | INT _v ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
    | ISNUMBER ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | ISZERO ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | LANGLE ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | LBRACE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | LETREC ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | LIST ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | LPAREN ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | MAXL ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | MIN ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | NEW ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | NEWREF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | NODE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | PAIR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | PROC ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | PROD ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | SELF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | SEND ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | SET ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | SETREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | SND ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | SUM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | SUPER ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | TL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | UNPAIR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState181
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState181

and _menhir_run183 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | AVG ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | BEGIN ->
        _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | CASET ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | CAST ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | CONS ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | DEBUG ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | DEREF ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | EMPTYLIST ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | EMPTYPRED ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | EMPTYTREE ->
        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | FST ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | HD ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | ID _v ->
        _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
    | IF ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | INSTANCEOF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | INT _v ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
    | ISNUMBER ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | ISZERO ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | LANGLE ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | LBRACE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | LETREC ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | LIST ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | LPAREN ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | MAXL ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | MIN ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | NEW ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | NEWREF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | NODE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | PAIR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | PROC ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | PROD ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | SELF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | SEND ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | SET ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | SETREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | SND ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | SUM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | SUPER ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | TL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | UNPAIR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183

and _menhir_run186 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | AVG ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | BEGIN ->
        _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | CASET ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | CAST ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | CONS ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | DEBUG ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | DEREF ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | EMPTYLIST ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | EMPTYPRED ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | EMPTYTREE ->
        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | FST ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | HD ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | ID _v ->
        _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | IF ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | INSTANCEOF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | INT _v ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | ISNUMBER ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | ISZERO ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | LANGLE ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | LBRACE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | LETREC ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | LIST ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | LPAREN ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | MAXL ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | MIN ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | NEW ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | NEWREF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | NODE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | PAIR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | PROC ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | PROD ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | SELF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | SEND ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | SET ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | SETREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | SND ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | SUM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | SUPER ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | TL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | UNPAIR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState186
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState186

and _menhir_run171 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALSMUTABLE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState173)
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | DIVIDED | DOT | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | EQUALS | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISNUMBER | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LLANGLE | LPAREN | MAXL | MIN | MINUS | NEW | NEWREF | NODE | OF | PAIR | PLUS | PROC | PROD | RANGLE | RBRACE | RPAREN | RRANGLE | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TIMES | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), (id : (
# 16 "lib/grammar.mly"
       (string)
# 1592 "lib/grammar.ml"
            ))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 169 "lib/grammar.mly"
                      ( Proj(e1,id) )
# 1597 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run177 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | AVG ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | BEGIN ->
        _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | CASET ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | CAST ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | CONS ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | DEBUG ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | DEREF ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | EMPTYLIST ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | EMPTYPRED ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | EMPTYTREE ->
        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | FST ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | HD ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | ID _v ->
        _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
    | IF ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | INSTANCEOF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | INT _v ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
    | ISNUMBER ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | ISZERO ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | LANGLE ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | LBRACE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | LETREC ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | LIST ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | LPAREN ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | MAXL ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | MIN ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | NEW ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | NEWREF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | NODE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | PAIR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | PROC ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | PROD ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | SELF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | SEND ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | SET ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | SETREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | SND ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | SUM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | SUPER ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | TL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | UNPAIR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState177
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState165 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (xs : (Ast.expr list))) = _menhir_stack in
            let _v : (Ast.expr) = let es = 
# 232 "<standard.mly>"
    ( xs )
# 1725 "lib/grammar.ml"
             in
            
# 132 "lib/grammar.mly"
                                                       ( Avg(es) )
# 1730 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RANGLE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (xs : (Ast.expr list))) = _menhir_stack in
            let _v : (Ast.expr) = let es = 
# 232 "<standard.mly>"
    ( xs )
# 1752 "lib/grammar.ml"
             in
            
# 160 "lib/grammar.mly"
                                                    ( Tuple(es) )
# 1757 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (xs : (Ast.expr list))) = _menhir_stack in
            let _v : (Ast.expr) = let es = 
# 232 "<standard.mly>"
    ( xs )
# 1779 "lib/grammar.ml"
             in
            
# 178 "lib/grammar.mly"
                                                        ( List(es))
# 1784 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (xs : (Ast.expr list))) = _menhir_stack in
            let _v : (Ast.expr) = let es = 
# 232 "<standard.mly>"
    ( xs )
# 1806 "lib/grammar.ml"
             in
            
# 133 "lib/grammar.mly"
                                                        ( Maxl(es) )
# 1811 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (id : (
# 16 "lib/grammar.mly"
       (string)
# 1832 "lib/grammar.ml"
            ))), _, (xs : (Ast.expr list))) = _menhir_stack in
            let _v : (Ast.expr) = let args = 
# 232 "<standard.mly>"
    ( xs )
# 1837 "lib/grammar.ml"
             in
            
# 172 "lib/grammar.mly"
         ( NewObject(id,args) )
# 1842 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (xs : (Ast.expr list))) = _menhir_stack in
            let _v : (Ast.expr) = let es = 
# 232 "<standard.mly>"
    ( xs )
# 1864 "lib/grammar.ml"
             in
            
# 131 "lib/grammar.mly"
                                                        ( Prod(es) )
# 1869 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState311 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e : (Ast.expr))), _, (id : (
# 16 "lib/grammar.mly"
       (string)
# 1890 "lib/grammar.ml"
            ))), _, (xs : (Ast.expr list))) = _menhir_stack in
            let _v : (Ast.expr) = let args = 
# 232 "<standard.mly>"
    ( xs )
# 1895 "lib/grammar.ml"
             in
            
# 175 "lib/grammar.mly"
         ( Send(e,id,args) )
# 1900 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (xs : (Ast.expr list))) = _menhir_stack in
            let _v : (Ast.expr) = let es = 
# 232 "<standard.mly>"
    ( xs )
# 1922 "lib/grammar.ml"
             in
            
# 130 "lib/grammar.mly"
                                                       ( Sum(es) )
# 1927 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (id : (
# 16 "lib/grammar.mly"
       (string)
# 1948 "lib/grammar.ml"
            ))), _, (xs : (Ast.expr list))) = _menhir_stack in
            let _v : (Ast.expr) = let args = 
# 232 "<standard.mly>"
    ( xs )
# 1953 "lib/grammar.ml"
             in
            
# 177 "lib/grammar.mly"
         ( Super(id,args) )
# 1958 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_type_annotation_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.texpr option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (id : (
# 16 "lib/grammar.mly"
       (string)
# 1980 "lib/grammar.ml"
        ))), _, (t : (Ast.texpr option))) = _menhir_stack in
        let _v : (string * Ast.texpr option) = 
# 231 "lib/grammar.mly"
                                     ( (id, t) )
# 1985 "lib/grammar.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (string * Ast.texpr option))) = _menhir_stack in
            let _v : ((string * Ast.texpr option) list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2009 "lib/grammar.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_formal_par_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ABS ->
                    _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | AVG ->
                    _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | BEGIN ->
                    _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | CASET ->
                    _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | CAST ->
                    _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | CONS ->
                    _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | DEBUG ->
                    _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | DEREF ->
                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | EMPTYLIST ->
                    _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | EMPTYPRED ->
                    _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | EMPTYTREE ->
                    _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | FST ->
                    _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | HD ->
                    _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | ID _v ->
                    _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
                | IF ->
                    _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | INSTANCEOF ->
                    _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | INT _v ->
                    _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
                | ISNUMBER ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | ISZERO ->
                    _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | LANGLE ->
                    _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | LBRACE ->
                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | LET ->
                    _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | LETREC ->
                    _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | LIST ->
                    _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | LPAREN ->
                    _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | MAXL ->
                    _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | MIN ->
                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | NEW ->
                    _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | NEWREF ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | NODE ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | PAIR ->
                    _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | PROC ->
                    _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | PROD ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | SELF ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | SEND ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | SET ->
                    _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | SETREF ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | SND ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | SUM ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | SUPER ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | TL ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | UNPAIR ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | EQUALS ->
                _menhir_reduce93 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Ast.texpr option) list) = 
# 142 "<standard.mly>"
    ( [] )
# 2269 "lib/grammar.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_formal_par__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "lib/grammar.mly"
       (string)
# 2276 "lib/grammar.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | COMMA | RPAREN ->
        _menhir_reduce93 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_goto_separated_nonempty_list_SEMICOLON_fieldtype_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.texpr) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((string * Ast.texpr) list)) = _v in
        let _v : ((string * Ast.texpr) list) = 
# 144 "<standard.mly>"
    ( x )
# 2302 "lib/grammar.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_fieldtype__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((string * Ast.texpr) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string * Ast.texpr))) = _menhir_stack in
        let _v : ((string * Ast.texpr) list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 2313 "lib/grammar.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_fieldtype_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run15 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.texpr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | INTTYPE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | LBRACE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | REFTYPE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | UNITTYPE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run17 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.texpr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | INTTYPE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | LBRACE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | REFTYPE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | UNITTYPE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_reduce69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.mdecl list) = 
# 211 "<standard.mly>"
    ( [] )
# 2372 "lib/grammar.ml"
     in
    _menhir_goto_list_method_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState61 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | RPAREN ->
                _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
        | ARROW | ID _ | TIMES ->
            _menhir_reduce109 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | INTTYPE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LBRACE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | REFTYPE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | UNITTYPE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_reduce93 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.texpr option) = 
# 114 "<standard.mly>"
    ( None )
# 2432 "lib/grammar.ml"
     in
    _menhir_goto_option_type_annotation_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | INTTYPE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LBRACE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | REFTYPE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | UNITTYPE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run117 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "lib/grammar.mly"
       (string)
# 2464 "lib/grammar.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | RPAREN ->
                _menhir_reduce93 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RANGLE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run126 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "lib/grammar.mly"
       (string)
# 2624 "lib/grammar.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
    | RANGLE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (
# 16 "lib/grammar.mly"
       (string)
# 2647 "lib/grammar.ml"
        ))) = _menhir_stack in
        let _v : (string list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2652 "lib/grammar.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_field__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * (bool * Ast.expr)) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (xs : ((string * (bool * Ast.expr)) list))) = _menhir_stack in
        let _v : (Ast.expr) = let fs = 
# 232 "<standard.mly>"
    ( xs )
# 2677 "lib/grammar.ml"
         in
        
# 168 "lib/grammar.mly"
                                                        ( Record(fs) )
# 2682 "lib/grammar.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run134 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "lib/grammar.mly"
       (string)
# 2695 "lib/grammar.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUALS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState252
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState252)
    | EQUALSMUTABLE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState167 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState168 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 128 "lib/grammar.mly"
                              ( Abs(e) )
# 2925 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168)
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState170
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | DIVIDED | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | EQUALS | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISNUMBER | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LLANGLE | LPAREN | MAXL | MIN | MINUS | NEW | NEWREF | NODE | OF | PAIR | PLUS | PROC | PROD | RANGLE | RBRACE | RPAREN | RRANGLE | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TIMES | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 126 "lib/grammar.mly"
                              ( Mul(e1,e2) )
# 2949 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170)
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState174
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISNUMBER | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | NEW | NEWREF | NODE | OF | PAIR | PROC | PROD | RANGLE | RBRACE | RPAREN | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), (id : (
# 16 "lib/grammar.mly"
       (string)
# 2980 "lib/grammar.ml"
            ))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 170 "lib/grammar.mly"
                                             ( SetField(e1,id,e) )
# 2985 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174)
    | MenhirState175 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | EQUALS | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISNUMBER | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LLANGLE | LPAREN | MAXL | MIN | MINUS | NEW | NEWREF | NODE | OF | PAIR | PLUS | PROC | PROD | RANGLE | RBRACE | RPAREN | RRANGLE | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 145 "lib/grammar.mly"
                                ( IsGT(e1,e2) )
# 3009 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176)
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | DIVIDED | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | EQUALS | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISNUMBER | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LLANGLE | LPAREN | MAXL | MIN | MINUS | NEW | NEWREF | NODE | OF | PAIR | PLUS | PROC | PROD | RANGLE | RBRACE | RPAREN | RRANGLE | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TIMES | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 127 "lib/grammar.mly"
                                ( Div(e1,e2) )
# 3029 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178)
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | EQUALS | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISNUMBER | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LLANGLE | LPAREN | MAXL | MIN | MINUS | NEW | NEWREF | NODE | OF | PAIR | PLUS | PROC | PROD | RANGLE | RBRACE | RPAREN | RRANGLE | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 124 "lib/grammar.mly"
                             ( Add(e1,e2) )
# 3053 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180)
    | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | EQUALS | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISNUMBER | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LLANGLE | LPAREN | MAXL | MIN | MINUS | NEW | NEWREF | NODE | OF | PAIR | PLUS | PROC | PROD | RANGLE | RBRACE | RPAREN | RRANGLE | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 125 "lib/grammar.mly"
                              ( Sub(e1,e2) )
# 3077 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182)
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | EQUALS | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISNUMBER | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LLANGLE | LPAREN | MAXL | MIN | MINUS | NEW | NEWREF | NODE | OF | PAIR | PLUS | PROC | PROD | RANGLE | RBRACE | RPAREN | RRANGLE | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 146 "lib/grammar.mly"
                                ( IsLT(e1,e2) )
# 3101 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState184)
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISNUMBER | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | NEW | NEWREF | NODE | OF | PAIR | PROC | PROD | RANGLE | RBRACE | RPAREN | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 144 "lib/grammar.mly"
                               ( IsEqual(e1,e2) )
# 3133 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187)
    | MenhirState78 | MenhirState80 | MenhirState311 | MenhirState91 | MenhirState106 | MenhirState110 | MenhirState115 | MenhirState136 | MenhirState192 | MenhirState165 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState191 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState192
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState192)
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | RANGLE | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3262 "lib/grammar.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191)
    | MenhirState198 | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState197 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState198)
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState197
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3391 "lib/grammar.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState197)
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | OF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState200 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EMPTYTREE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ARROW ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | ABS ->
                            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | AVG ->
                            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | BEGIN ->
                            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | CASET ->
                            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | CAST ->
                            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | CONS ->
                            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | DEBUG ->
                            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | DEREF ->
                            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | EMPTYLIST ->
                            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | EMPTYPRED ->
                            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | EMPTYTREE ->
                            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | FST ->
                            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | HD ->
                            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | ID _v ->
                            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState204 _v
                        | IF ->
                            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | INSTANCEOF ->
                            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | INT _v ->
                            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState204 _v
                        | ISNUMBER ->
                            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | ISZERO ->
                            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | LANGLE ->
                            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | LBRACE ->
                            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | LET ->
                            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | LETREC ->
                            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | LIST ->
                            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | LPAREN ->
                            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | MAXL ->
                            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | MIN ->
                            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | NEW ->
                            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | NEWREF ->
                            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | NODE ->
                            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | PAIR ->
                            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | PROC ->
                            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | PROD ->
                            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | SELF ->
                            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | SEND ->
                            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | SET ->
                            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | SETREF ->
                            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | SND ->
                            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | SUM ->
                            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | SUPER ->
                            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | TL ->
                            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | UNPAIR ->
                            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState204
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState204)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState200)
    | MenhirState204 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState205 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | NODE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ID _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = (_menhir_stack, _v) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | COMMA ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | ID _v ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_stack = (_menhir_stack, _v) in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | COMMA ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_env = _menhir_discard _menhir_env in
                                    let _tok = _menhir_env._menhir_token in
                                    (match _tok with
                                    | ID _v ->
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let _menhir_stack = (_menhir_stack, _v) in
                                        let _menhir_env = _menhir_discard _menhir_env in
                                        let _tok = _menhir_env._menhir_token in
                                        (match _tok with
                                        | RPAREN ->
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let _menhir_env = _menhir_discard _menhir_env in
                                            let _tok = _menhir_env._menhir_token in
                                            (match _tok with
                                            | ARROW ->
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let _menhir_env = _menhir_discard _menhir_env in
                                                let _tok = _menhir_env._menhir_token in
                                                (match _tok with
                                                | ABS ->
                                                    _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | AVG ->
                                                    _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | BEGIN ->
                                                    _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | CASET ->
                                                    _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | CAST ->
                                                    _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | CONS ->
                                                    _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | DEBUG ->
                                                    _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | DEREF ->
                                                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | EMPTYLIST ->
                                                    _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | EMPTYPRED ->
                                                    _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | EMPTYTREE ->
                                                    _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | FST ->
                                                    _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | HD ->
                                                    _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | ID _v ->
                                                    _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState215 _v
                                                | IF ->
                                                    _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | INSTANCEOF ->
                                                    _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | INT _v ->
                                                    _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState215 _v
                                                | ISNUMBER ->
                                                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | ISZERO ->
                                                    _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | LANGLE ->
                                                    _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | LBRACE ->
                                                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | LET ->
                                                    _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | LETREC ->
                                                    _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | LIST ->
                                                    _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | LPAREN ->
                                                    _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | MAXL ->
                                                    _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | MIN ->
                                                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | NEW ->
                                                    _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | NEWREF ->
                                                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | NODE ->
                                                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | PAIR ->
                                                    _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | PROC ->
                                                    _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | PROD ->
                                                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | SELF ->
                                                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | SEND ->
                                                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | SET ->
                                                    _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | SETREF ->
                                                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | SND ->
                                                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | SUM ->
                                                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | SUPER ->
                                                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | TL ->
                                                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | UNPAIR ->
                                                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState215
                                                | _ ->
                                                    assert (not _menhir_env._menhir_error);
                                                    _menhir_env._menhir_error <- true;
                                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState215)
                                            | _ ->
                                                assert (not _menhir_env._menhir_error);
                                                _menhir_env._menhir_error <- true;
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
                                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                        | _ ->
                                            assert (not _menhir_env._menhir_error);
                                            _menhir_env._menhir_error <- true;
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
                                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState205
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205)
    | MenhirState215 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState216
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState216
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState216
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState216
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState216
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState216
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState216 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))), _), (id1 : (
# 16 "lib/grammar.mly"
       (string)
# 3798 "lib/grammar.ml"
            ))), (id2 : (
# 16 "lib/grammar.mly"
       (string)
# 3802 "lib/grammar.ml"
            ))), (id3 : (
# 16 "lib/grammar.mly"
       (string)
# 3806 "lib/grammar.ml"
            ))), _, (e3 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 167 "lib/grammar.mly"
                              ( CaseT(e1,e2,id1,id2,id3,e3) )
# 3811 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState216
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState216
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState216)
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState218 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | RPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), (id : (
# 16 "lib/grammar.mly"
       (string)
# 3847 "lib/grammar.ml"
                    ))) = _menhir_stack in
                    let _v : (Ast.expr) = 
# 185 "lib/grammar.mly"
                                              ( Cast(e1,id) )
# 3852 "lib/grammar.ml"
                     in
                    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState218)
    | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState222 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState223 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState223 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState223
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState223)
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState222
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState222
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState222
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState222
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState222
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState222
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState222
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState222
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState222)
    | MenhirState223 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState224 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 183 "lib/grammar.mly"
                                                    ( Cons(e1,e2) )
# 4033 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState224)
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState226
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState226
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState226
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState226
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState226
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState226
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState226 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 123 "lib/grammar.mly"
                                ( Debug(e) )
# 4070 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState226
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState226
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState226)
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState228
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState228
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState228
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState228
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState228
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState228
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState228 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 148 "lib/grammar.mly"
                                  ( DeRef(e) )
# 4107 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState228
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState228
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState228)
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState230 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 182 "lib/grammar.mly"
                                      ( IsEmpty(e) )
# 4144 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState230)
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState232 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 135 "lib/grammar.mly"
                              ( Fst(e) )
# 4181 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState232)
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState234 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 180 "lib/grammar.mly"
                               ( Hd(e) )
# 4218 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState234
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState234)
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState236 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState237 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState237 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState237
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState237)
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState236
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState236)
    | MenhirState237 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState238 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState239 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState239 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState239
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState239)
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState238
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState238)
    | MenhirState239 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISNUMBER | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | NEW | NEWREF | NODE | OF | PAIR | PROC | PROD | RANGLE | RBRACE | RPAREN | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))), _), _, (e3 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 150 "lib/grammar.mly"
                                                  ( ITE(e1,e2,e3) )
# 4494 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState240)
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState241 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | RPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((((_menhir_stack, _menhir_s), _, (e : (Ast.expr))), _), (id : (
# 16 "lib/grammar.mly"
       (string)
# 4526 "lib/grammar.ml"
                    ))) = _menhir_stack in
                    let _v : (Ast.expr) = 
# 184 "lib/grammar.mly"
                                                  ( IsInstanceOf(e,id) )
# 4531 "lib/grammar.ml"
                     in
                    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState241
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState241)
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState245
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState245
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState245
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState245
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState245
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState245
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState245 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 143 "lib/grammar.mly"
                                     ( IsNumber(e) )
# 4592 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState245
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState245
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState245)
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState247
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState247
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState247
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState247
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState247
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState247
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState247 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 142 "lib/grammar.mly"
                                   ( IsZero(e) )
# 4629 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState247
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState247
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState247)
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState251
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState251
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState251
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState251
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState251
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState251
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState251
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState251
        | RBRACE | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (id : (
# 16 "lib/grammar.mly"
       (string)
# 4666 "lib/grammar.ml"
            ))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (string * (bool * Ast.expr)) = 
# 197 "lib/grammar.mly"
                                     ( (id,(true,e)) )
# 4671 "lib/grammar.ml"
             in
            _menhir_goto_field _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState251)
    | MenhirState252 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState253
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState253
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState253
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState253
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState253
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState253
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState253
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState253
        | RBRACE | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (id : (
# 16 "lib/grammar.mly"
       (string)
# 4704 "lib/grammar.ml"
            ))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (string * (bool * Ast.expr)) = 
# 196 "lib/grammar.mly"
                              ( (id,(false,e)) )
# 4709 "lib/grammar.ml"
             in
            _menhir_goto_field _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState253)
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState260
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState260
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState260
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState260 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState261
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState261)
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState260
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState260
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState260
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState260
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState260
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState260)
    | MenhirState261 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISNUMBER | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | NEW | NEWREF | NODE | OF | PAIR | PROC | PROD | RANGLE | RBRACE | RPAREN | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, (xs : (string list))), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = let is = 
# 232 "<standard.mly>"
    ( xs )
# 4861 "lib/grammar.ml"
             in
            
# 162 "lib/grammar.mly"
            ( Untuple(is,e1,e2) )
# 4866 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState262)
    | MenhirState264 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState265
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState265
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState265
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState265 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState266)
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState265
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState265
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState265
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState265
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState265
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState265)
    | MenhirState266 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState267
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState267
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState267
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState267
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState267
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState267
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState267
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISNUMBER | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | NEW | NEWREF | NODE | OF | PAIR | PROC | PROD | RANGLE | RBRACE | RPAREN | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (x : (
# 16 "lib/grammar.mly"
       (string)
# 5017 "lib/grammar.ml"
            ))), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 137 "lib/grammar.mly"
                                                ( Let(x,e1,e2) )
# 5022 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState267)
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState268
        | ID _ | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (x : (
# 16 "lib/grammar.mly"
       (string)
# 5055 "lib/grammar.ml"
            ))), (y : (
# 16 "lib/grammar.mly"
       (string)
# 5059 "lib/grammar.ml"
            ))), _, (targ : (Ast.texpr option))), _, (tres : (Ast.texpr option))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (string * string * Ast.texpr option * Ast.texpr option * Ast.expr) = 
# 190 "lib/grammar.mly"
            ( (x, y, targ, tres, e) )
# 5064 "lib/grammar.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState269 _v
            | IN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * string * Ast.texpr option * Ast.texpr option * Ast.expr))) = _menhir_stack in
                let _v : (Ast.rdecs) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 5079 "lib/grammar.ml"
                 in
                _menhir_goto_nonempty_list_rdecs_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState269)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState268)
    | MenhirState272 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISNUMBER | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | NEW | NEWREF | NODE | OF | PAIR | PROC | PROD | RANGLE | RBRACE | RPAREN | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (decs : (Ast.rdecs))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 138 "lib/grammar.mly"
                                                    ( Letrec(decs, e) )
# 5115 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState273)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState276 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 155 "lib/grammar.mly"
                                  ( Sub(Int 0, e) )
# 5148 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState276
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState276)
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState278 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState280 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState280 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState280
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState280)
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState278 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState278 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState278 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 153 "lib/grammar.mly"
                           (e)
# 5346 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState278
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState278)
    | MenhirState280 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState281
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState281
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState281
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState281
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState281
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState281
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState281 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 157 "lib/grammar.mly"
                                              ( Pair(e1,e2) )
# 5401 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState281
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState281
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState281)
    | MenhirState278 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState283 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 141 "lib/grammar.mly"
                                       ( App(e1,e2) )
# 5438 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState283)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState287 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState288 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState288 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState288)
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState287
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState287
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState287
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState287
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState287
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState287
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState287
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState287
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState287)
    | MenhirState288 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState289
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState289
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState289
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState289
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState289
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState289
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState289 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 129 "lib/grammar.mly"
                                               ( Min(e1,e2) )
# 5595 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState289
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState289
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState289)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState293
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState293
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState293
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState293
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState293
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState293
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState293 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 147 "lib/grammar.mly"
                                   ( NewRef(e) )
# 5632 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState293
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState293
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState293)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState295 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState296
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState296)
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState295
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState295
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState295
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState295
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState295
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState295
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState295
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState295
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState295)
    | MenhirState296 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState297 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState298 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState298 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState298
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState298)
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState297
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState297
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState297
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState297
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState297
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState297
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState297
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState297
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState297)
    | MenhirState298 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState299
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState299
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState299
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState299
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState299
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState299
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState299 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))), _), _, (e3 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 164 "lib/grammar.mly"
                                                                  ( Node(e1,e2,e3) )
# 5909 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState299
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState299
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState299)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState301 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState302)
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState301
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState301
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState301
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState301
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState301
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState301
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState301
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState301
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState301)
    | MenhirState302 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState303
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState303
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState303
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState303
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState303
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState303
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState303 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 134 "lib/grammar.mly"
                                                ( Pair(e1,e2) )
# 6066 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState303
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState303
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState303)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState305
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState305
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState305
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState305
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState305
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState305
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState305 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (x : (
# 16 "lib/grammar.mly"
       (string)
# 6102 "lib/grammar.ml"
            ))), _, (t : (Ast.texpr option))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 140 "lib/grammar.mly"
         ( Proc(x,t,e) )
# 6107 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState305
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState305
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState305)
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState309
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState309
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState309
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState309 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ABS ->
                    _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | AVG ->
                    _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | BEGIN ->
                    _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | CASET ->
                    _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | CAST ->
                    _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | CONS ->
                    _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | DEBUG ->
                    _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | DEREF ->
                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | EMPTYLIST ->
                    _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | EMPTYPRED ->
                    _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | EMPTYTREE ->
                    _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | FST ->
                    _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | HD ->
                    _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | ID _v ->
                    _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
                | IF ->
                    _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | INSTANCEOF ->
                    _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | INT _v ->
                    _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
                | ISNUMBER ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | ISZERO ->
                    _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | LANGLE ->
                    _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | LBRACE ->
                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | LET ->
                    _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | LETREC ->
                    _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | LIST ->
                    _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | LPAREN ->
                    _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | MAXL ->
                    _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | MIN ->
                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | NEW ->
                    _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | NEWREF ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | NODE ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | PAIR ->
                    _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | PROC ->
                    _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | PROD ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | SELF ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | SEND ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | SET ->
                    _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | SETREF ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | SND ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | SUM ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | SUPER ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | TL ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | UNPAIR ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | RPAREN ->
                    _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState311
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState311)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState309
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState309
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState309
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState309
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState309
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState309)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState314
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState314
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState314
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState314
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState314
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState314
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState314
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISNUMBER | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | NEW | NEWREF | NODE | OF | PAIR | PROC | PROD | RANGLE | RBRACE | RPAREN | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (x : (
# 16 "lib/grammar.mly"
       (string)
# 6275 "lib/grammar.ml"
            ))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 151 "lib/grammar.mly"
                                ( Set(x,e) )
# 6280 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState314)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState315 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState316 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState316 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState316
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState316)
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState315
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState315
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState315
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState315
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState315
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState315
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState315
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState315
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState315)
    | MenhirState316 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState317
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState317
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState317
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState317
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState317
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState317
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState317 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 149 "lib/grammar.mly"
                                                      ( SetRef(e1,e2) )
# 6433 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState317
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState317
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState317)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState319
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState319
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState319
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState319
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState319
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState319
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState319 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 136 "lib/grammar.mly"
                              ( Snd(e) )
# 6470 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState319
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState319
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState319)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState325
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState325
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState325
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState325
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState325
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState325
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState325 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 181 "lib/grammar.mly"
                               ( Tl(e) )
# 6507 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState325
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState325
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState325)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState327
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState327
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState327
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState327 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState328 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState328 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState328
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState328)
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState327
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState327
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState327
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState327
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState327
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState327)
    | MenhirState328 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState329
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState329
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState329
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState329
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState329
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState329
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState329
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISNUMBER | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | NEW | NEWREF | NODE | OF | PAIR | PROC | PROD | RANGLE | RBRACE | RPAREN | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (x : (
# 16 "lib/grammar.mly"
       (string)
# 6662 "lib/grammar.ml"
            ))), (y : (
# 16 "lib/grammar.mly"
       (string)
# 6666 "lib/grammar.ml"
            ))), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 159 "lib/grammar.mly"
                ( Unpair(x,y,e1,e2) )
# 6671 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState329)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState330 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (id : (
# 16 "lib/grammar.mly"
       (string)
# 6703 "lib/grammar.ml"
            ))), _, (xs : ((string * Ast.texpr option) list))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.mdecl) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 6708 "lib/grammar.ml"
             in
            
# 220 "lib/grammar.mly"
         ( Method(id,None,params,e) )
# 6713 "lib/grammar.ml"
             in
            _menhir_goto_method_decl _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState330
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState330)
    | MenhirState337 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState338
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState338
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState338
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState338
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState338
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState338
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState338 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, (retType : (Ast.texpr))), (id : (
# 16 "lib/grammar.mly"
       (string)
# 6749 "lib/grammar.ml"
            ))), _, (xs : ((string * Ast.texpr option) list))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.mdecl) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 6754 "lib/grammar.ml"
             in
            
# 224 "lib/grammar.mly"
       ( Method(id,Some retType,params,e) )
# 6759 "lib/grammar.ml"
             in
            _menhir_goto_method_decl _menhir_env _menhir_stack _menhir_s _v
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState338
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState338
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState338)
    | MenhirState346 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState347
        | DOT ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState347
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState347 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (cls : (Ast.cdecl list))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.prog) = 
# 116 "lib/grammar.mly"
                                                  ( AProg(cls,e) )
# 6787 "lib/grammar.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.prog)) = _v in
            Obj.magic _1
        | EQUALS ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState347
        | LLANGLE ->
            _menhir_run183 _menhir_env (Obj.magic _menhir_stack) MenhirState347
        | MINUS ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState347
        | PLUS ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState347
        | RRANGLE ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState347
        | TIMES ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState347
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState347)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (xs : (Ast.expr list))) = _menhir_stack in
        let _v : (Ast.expr) = let es = 
# 232 "<standard.mly>"
    ( xs )
# 6827 "lib/grammar.ml"
         in
        
# 152 "lib/grammar.mly"
                                                   ( BeginEnd(es) )
# 6832 "lib/grammar.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expr list) = 
# 142 "<standard.mly>"
    ( [] )
# 6847 "lib/grammar.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_iface_or_class_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.cdecl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState349
    | INTERFACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState349
    | ABS | AVG | BEGIN | CASET | CAST | CONS | DEBUG | DEREF | EMPTYLIST | EMPTYPRED | EMPTYTREE | FST | HD | ID _ | IF | INSTANCEOF | INT _ | ISNUMBER | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | NEW | NEWREF | NODE | PAIR | PROC | PROD | SELF | SEND | SET | SETREF | SND | SUM | SUPER | TL | UNPAIR ->
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState349
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState349

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_fieldtype__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.texpr) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (xs : ((string * Ast.texpr) list))) = _menhir_stack in
        let _v : (Ast.texpr) = let ts = 
# 232 "<standard.mly>"
    ( xs )
# 6884 "lib/grammar.ml"
         in
        
# 242 "lib/grammar.mly"
                                                            ( RecordType(ts) )
# 6889 "lib/grammar.ml"
         in
        _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "lib/grammar.mly"
       (string)
# 6902 "lib/grammar.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOLTYPE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | ID _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | INTTYPE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | LBRACE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | REFTYPE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | UNITTYPE ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_texpr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.texpr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (id : (
# 16 "lib/grammar.mly"
       (string)
# 6957 "lib/grammar.ml"
            ))), _, (t : (Ast.texpr))) = _menhir_stack in
            let _v : (string * Ast.texpr) = 
# 200 "lib/grammar.mly"
                          ( (id,t) )
# 6962 "lib/grammar.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * Ast.texpr))) = _menhir_stack in
                let _v : ((string * Ast.texpr) list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 6986 "lib/grammar.ml"
                 in
                _menhir_goto_separated_nonempty_list_SEMICOLON_fieldtype_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (t1 : (Ast.texpr))), _, (t2 : (Ast.texpr))) = _menhir_stack in
        let _v : (Ast.texpr) = 
# 239 "lib/grammar.mly"
                                ( PairType(t1,t2) )
# 7008 "lib/grammar.ml"
         in
        _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQUALS | ID _ | RBRACE | RPAREN | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (t1 : (Ast.texpr))), _, (t2 : (Ast.texpr))) = _menhir_stack in
            let _v : (Ast.texpr) = 
# 238 "lib/grammar.mly"
                               ( FuncType(t1,t2) )
# 7026 "lib/grammar.ml"
             in
            _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t1 : (Ast.texpr))) = _menhir_stack in
            let _v : (Ast.texpr) = 
# 240 "lib/grammar.mly"
                             ( t1 )
# 7050 "lib/grammar.ml"
             in
            _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (t1 : (Ast.texpr))) = _menhir_stack in
        let _v : (Ast.texpr) = 
# 241 "lib/grammar.mly"
                    ( RefType(t1) )
# 7068 "lib/grammar.ml"
         in
        _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
                | RPAREN ->
                    _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | TIMES ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQUALS | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t : (Ast.texpr))) = _menhir_stack in
            let _v : (Ast.texpr) = 
# 193 "lib/grammar.mly"
                 ( t )
# 7126 "lib/grammar.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (x : (Ast.texpr)) = _v in
            let _v : (Ast.texpr option) = 
# 116 "<standard.mly>"
    ( Some x )
# 7134 "lib/grammar.ml"
             in
            _menhir_goto_option_type_annotation_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (id : (
# 16 "lib/grammar.mly"
       (string)
# 7157 "lib/grammar.ml"
            )) = _v in
            let ((_menhir_stack, _menhir_s), _, (t : (Ast.texpr))) = _menhir_stack in
            let _v : (string * Ast.texpr option) = 
# 214 "lib/grammar.mly"
                        ( (id,Some t) )
# 7163 "lib/grammar.ml"
             in
            _menhir_goto_obj_fields _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState334 _v
                | RPAREN ->
                    _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState334
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState334)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | TIMES ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_obj_fields_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.texpr option) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Ast.texpr option))), _, (xs : ((string * Ast.texpr option) list))) = _menhir_stack in
        let _v : ((string * Ast.texpr option) list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 7228 "lib/grammar.ml"
         in
        _menhir_goto_list_obj_fields_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | METHOD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | RBRACE ->
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | _ ->
        _menhir_fail ()

and _menhir_goto_obj_fields : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Ast.texpr option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FIELD ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | METHOD | RBRACE ->
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_reduce109 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 16 "lib/grammar.mly"
       (string)
# 7266 "lib/grammar.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id : (
# 16 "lib/grammar.mly"
       (string)
# 7272 "lib/grammar.ml"
    ))) = _menhir_stack in
    let _v : (Ast.texpr) = 
# 234 "lib/grammar.mly"
        ( UserType(id) )
# 7277 "lib/grammar.ml"
     in
    _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | RPAREN ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | EQUALS ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | ABS ->
                                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | AVG ->
                                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | BEGIN ->
                                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | CASET ->
                                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | CAST ->
                                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | CONS ->
                                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | DEBUG ->
                                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | DEREF ->
                                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | EMPTYLIST ->
                                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | EMPTYPRED ->
                                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | EMPTYTREE ->
                                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | FST ->
                                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | HD ->
                                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | ID _v ->
                                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
                            | IF ->
                                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | INSTANCEOF ->
                                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | INT _v ->
                                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
                            | ISNUMBER ->
                                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | ISZERO ->
                                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | LANGLE ->
                                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | LBRACE ->
                                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | LET ->
                                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | LETREC ->
                                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | LIST ->
                                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | LPAREN ->
                                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | MAXL ->
                                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | MIN ->
                                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | NEW ->
                                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | NEWREF ->
                                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | NODE ->
                                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | PAIR ->
                                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | PROC ->
                                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | PROD ->
                                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | SELF ->
                                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | SEND ->
                                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | SET ->
                                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | SETREF ->
                                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | SND ->
                                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | SUM ->
                                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | SUPER ->
                                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | TL ->
                                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | UNPAIR ->
                                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | RPAREN ->
                _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | RPAREN ->
            _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run81 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run85 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run88 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | AVG ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | BEGIN ->
        _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | CASET ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | CAST ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | CONS ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | DEBUG ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | DEREF ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | EMPTYLIST ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | EMPTYPRED ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | EMPTYTREE ->
        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | FST ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | HD ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | ID _v ->
        _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | IF ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | INSTANCEOF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | INT _v ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | ISNUMBER ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | ISZERO ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LANGLE ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LBRACE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LETREC ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LIST ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LPAREN ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | MAXL ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | MIN ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NEW ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NEWREF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NODE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | PAIR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | PROC ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | PROD ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | SELF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | SEND ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | SET ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | SETREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | SND ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | SUM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | SUPER ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | TL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | UNPAIR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run89 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.expr) = 
# 173 "lib/grammar.mly"
        ( Self )
# 8215 "lib/grammar.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run90 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | RPAREN ->
            _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run92 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | RPAREN ->
                _menhir_reduce93 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run98 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run100 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run102 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run104 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | RPAREN ->
                _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run107 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run109 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | RPAREN ->
            _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run111 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | AVG ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | BEGIN ->
        _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | CASET ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | CAST ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | CONS ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | DEBUG ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | DEREF ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | EMPTYLIST ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | EMPTYPRED ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | EMPTYTREE ->
        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | FST ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | HD ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | ID _v ->
        _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | IF ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | INSTANCEOF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | INT _v ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | ISNUMBER ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | ISZERO ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LANGLE ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LBRACE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LETREC ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LIST ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LPAREN ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | MAXL ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | MIN ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | MINUS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState111 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
    | NEW ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NEWREF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NODE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | PAIR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | PROC ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | PROD ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState111 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Ast.expr) = 
# 156 "lib/grammar.mly"
                 ( Unit )
# 9193 "lib/grammar.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | SELF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | SEND ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | SET ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | SETREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | SND ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | SUM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | SUPER ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | TL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | UNPAIR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111

and _menhir_run114 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | RPAREN ->
            _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run116 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116

and _menhir_run124 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | AVG ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | BEGIN ->
                _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | CASET ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | CAST ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | CONS ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | DEBUG ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | DEREF ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | EMPTYLIST ->
                _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | EMPTYPRED ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | EMPTYTREE ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | FST ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | HD ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | ID _v ->
                _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | INSTANCEOF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | INT _v ->
                _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
            | ISNUMBER ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState264
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState264)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | LANGLE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
        | RANGLE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState125 in
            let _v : (string list) = 
# 142 "<standard.mly>"
    ( [] )
# 9464 "lib/grammar.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run133 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState133 in
        let _v : ((string * (bool * Ast.expr)) list) = 
# 142 "<standard.mly>"
    ( [] )
# 9492 "lib/grammar.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_field__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133

and _menhir_run136 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | AVG ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | BEGIN ->
        _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | CASET ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | CAST ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | CONS ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | DEBUG ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | DEREF ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | EMPTYLIST ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | EMPTYPRED ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | EMPTYTREE ->
        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | FST ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | HD ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | ID _v ->
        _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | IF ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | INSTANCEOF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | INT _v ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | ISNUMBER ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | ISZERO ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | LANGLE ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | LBRACE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | LETREC ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | LIST ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | LPAREN ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | MAXL ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | MIN ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | NEW ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | NEWREF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | NODE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | PAIR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | PROC ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | PROD ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | SELF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | SEND ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | SET ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | SETREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | SND ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | SUM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | SUPER ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | TL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | UNPAIR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | RANGLE ->
        _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136

and _menhir_run137 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run139 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run141 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 15 "lib/grammar.mly"
       (int)
# 9812 "lib/grammar.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 15 "lib/grammar.mly"
       (int)
# 9820 "lib/grammar.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 121 "lib/grammar.mly"
          ( Int i )
# 9825 "lib/grammar.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run142 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run144 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | AVG ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | BEGIN ->
        _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | CASET ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | CAST ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | CONS ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | DEBUG ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | DEREF ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | EMPTYLIST ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | EMPTYPRED ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | EMPTYTREE ->
        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | FST ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | HD ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | ID _v ->
        _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | IF ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | INSTANCEOF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | INT _v ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | ISNUMBER ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | ISZERO ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | LANGLE ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | LBRACE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | LETREC ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | LIST ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | LPAREN ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | MAXL ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | MIN ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | NEW ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | NEWREF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | NODE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | PAIR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | PROC ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | PROD ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | SELF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | SEND ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | SET ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | SETREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | SND ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | SUM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | SUPER ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | TL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | UNPAIR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144

and _menhir_run145 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "lib/grammar.mly"
       (string)
# 10033 "lib/grammar.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (
# 16 "lib/grammar.mly"
       (string)
# 10041 "lib/grammar.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 122 "lib/grammar.mly"
         ( Var x )
# 10046 "lib/grammar.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run146 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run148 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run150 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.expr) = 
# 163 "lib/grammar.mly"
            ( EmptyTree )
# 10269 "lib/grammar.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run151 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run153 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.expr) = 
# 179 "lib/grammar.mly"
            ( EmptyList )
# 10386 "lib/grammar.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run154 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run156 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run158 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run160 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState161
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run162 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | AVG ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | BEGIN ->
        _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | CASET ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | CAST ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | CONS ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | DEBUG ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | DEREF ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | EMPTYLIST ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | EMPTYPRED ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | EMPTYTREE ->
        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | FST ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | HD ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | ID _v ->
        _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
    | IF ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | INSTANCEOF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | INT _v ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
    | ISNUMBER ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | ISZERO ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | LANGLE ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | LBRACE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | LETREC ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | LIST ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | LPAREN ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | MAXL ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | MIN ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | NEW ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | NEWREF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | NODE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | PAIR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | PROC ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | PROD ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | SELF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | SEND ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | SET ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | SETREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | SND ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | SUM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | SUPER ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | TL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | UNPAIR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162

and _menhir_run163 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | AVG ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | BEGIN ->
        _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | CASET ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | CAST ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | CONS ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | DEBUG ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | DEREF ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | EMPTYLIST ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | EMPTYPRED ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | EMPTYTREE ->
        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | FST ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | HD ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | ID _v ->
        _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
    | IF ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | INSTANCEOF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | INT _v ->
        _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
    | ISNUMBER ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | ISZERO ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | LANGLE ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | LBRACE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | LETREC ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | LIST ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | LPAREN ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | MAXL ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | MIN ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | NEW ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | NEWREF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | NODE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | PAIR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | PROC ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | PROD ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | SELF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | SEND ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | SET ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | SETREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | SND ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | SUM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | SUPER ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | TL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | UNPAIR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState163 in
        let _v : (Ast.expr list) = 
# 142 "<standard.mly>"
    ( [] )
# 11005 "lib/grammar.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163

and _menhir_run164 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | RPAREN ->
            _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState165)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run166 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState167
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState167)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_abstract_method_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.abs_mdecl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (id : (
# 16 "lib/grammar.mly"
       (string)
# 11243 "lib/grammar.ml"
            ))), _, (amths : (Ast.abs_mdecl list))) = _menhir_stack in
            let _v : (Ast.cdecl) = 
# 207 "lib/grammar.mly"
  ( Interface(id,amths))
# 11248 "lib/grammar.ml"
             in
            _menhir_goto_iface_or_class_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.abs_mdecl))), _, (xs : (Ast.abs_mdecl list))) = _menhir_stack in
        let _v : (Ast.abs_mdecl list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 11264 "lib/grammar.ml"
         in
        _menhir_goto_list_abstract_method_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.texpr) = 
# 237 "lib/grammar.mly"
         ( UnitType )
# 11277 "lib/grammar.ml"
     in
    _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | INTTYPE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LBRACE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | REFTYPE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | UNITTYPE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | INTTYPE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LBRACE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | REFTYPE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | UNITTYPE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState8 in
        let _v : ((string * Ast.texpr) list) = 
# 142 "<standard.mly>"
    ( [] )
# 11345 "lib/grammar.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_fieldtype__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.texpr) = 
# 235 "lib/grammar.mly"
        ( IntType )
# 11360 "lib/grammar.ml"
     in
    _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "lib/grammar.mly"
       (string)
# 11367 "lib/grammar.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce109 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.texpr) = 
# 236 "lib/grammar.mly"
         ( BoolType )
# 11381 "lib/grammar.ml"
     in
    _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Ast.texpr option) list) = 
# 211 "<standard.mly>"
    ( [] )
# 11390 "lib/grammar.ml"
     in
    _menhir_goto_list_obj_fields_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState54 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW | ID _ | TIMES ->
            _menhir_reduce109 _menhir_env (Obj.magic _menhir_stack)
        | FIELD | METHOD | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (id : (
# 16 "lib/grammar.mly"
       (string)
# 11416 "lib/grammar.ml"
            ))) = _menhir_stack in
            let _v : (string * Ast.texpr option) = 
# 213 "lib/grammar.mly"
               ( (id,None) )
# 11421 "lib/grammar.ml"
             in
            _menhir_goto_obj_fields _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | INTTYPE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LBRACE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | REFTYPE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | UNITTYPE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_goto_list_iface_or_class_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.cdecl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | AVG ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | BEGIN ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | CASET ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | CAST ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | CONS ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | DEBUG ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | DEREF ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | EMPTYLIST ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | EMPTYPRED ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | EMPTYTREE ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | FST ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | HD ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | ID _v ->
            _menhir_run145 _menhir_env (Obj.magic _menhir_stack) MenhirState346 _v
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | INSTANCEOF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | INT _v ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack) MenhirState346 _v
        | ISNUMBER ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState346
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState346)
    | MenhirState349 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.cdecl))), _, (xs : (Ast.cdecl list))) = _menhir_stack in
        let _v : (Ast.cdecl list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 11549 "lib/grammar.ml"
         in
        _menhir_goto_list_iface_or_class_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.abs_mdecl list) = 
# 211 "<standard.mly>"
    ( [] )
# 11560 "lib/grammar.ml"
     in
    _menhir_goto_list_abstract_method_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | INTTYPE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LBRACE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | REFTYPE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | UNITTYPE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_goto_option_implements_declaration_ : _menhir_env -> 'ttv_tail -> (string option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FIELD ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | METHOD | RBRACE ->
            _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState349 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState347 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState346 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState340 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState338 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState337 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState334 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState330 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState329 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState328 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState327 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState325 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState319 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState317 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState316 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState315 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState314 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState311 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState309 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState305 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState303 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState302 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState301 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState299 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState298 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState297 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState296 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState295 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState293 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState289 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState288 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState287 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState283 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState281 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState280 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState278 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState276 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState273 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState272 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState269 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState268 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState267 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState266 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState265 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState264 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState262 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState261 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState260 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState258 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState253 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState252 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState251 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState247 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState245 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState241 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState240 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState239 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState238 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState237 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState236 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState234 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState232 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState230 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState228 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState226 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState224 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState223 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState222 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState218 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState216 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState215 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState204 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState200 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState197 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState178 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState175 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState167 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState165 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.cdecl list) = 
# 211 "<standard.mly>"
    ( [] )
# 12272 "lib/grammar.ml"
     in
    _menhir_goto_list_iface_or_class_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | METHOD ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | RBRACE ->
                _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EXTENDS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IMPLEMENTS ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ID _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (id : (
# 16 "lib/grammar.mly"
       (string)
# 12349 "lib/grammar.ml"
                        )) = _v in
                        let _v : (string) = 
# 210 "lib/grammar.mly"
                    ( id )
# 12354 "lib/grammar.ml"
                         in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (x : (string)) = _v in
                        let _v : (string option) = 
# 116 "<standard.mly>"
    ( Some x )
# 12362 "lib/grammar.ml"
                         in
                        _menhir_goto_option_implements_declaration_ _menhir_env _menhir_stack _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | LBRACE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _v : (string option) = 
# 114 "<standard.mly>"
    ( None )
# 12375 "lib/grammar.ml"
                     in
                    _menhir_goto_option_implements_declaration_ _menhir_env _menhir_stack _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.prog) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INTERFACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ABS | AVG | BEGIN | CASET | CAST | CONS | DEBUG | DEREF | EMPTYLIST | EMPTYPRED | EMPTYTREE | FST | HD | ID _ | IF | INSTANCEOF | INT _ | ISNUMBER | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | NEW | NEWREF | NODE | PAIR | PROC | PROD | SELF | SEND | SET | SETREF | SND | SUM | SUPER | TL | UNPAIR ->
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 12441 "lib/grammar.ml"
