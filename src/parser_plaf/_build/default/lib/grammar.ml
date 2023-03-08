
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
    | LIST
    | LETREC
    | LET
    | LBRACE
    | LANGLE
    | ISZERO
    | INTTYPE
    | INTERFACE
    | INT of (
# 15 "lib/grammar.mly"
       (int)
# 52 "lib/grammar.ml"
  )
    | INSTANCEOF
    | IN
    | IMPLEMENTS
    | IF
    | ID of (
# 16 "lib/grammar.mly"
       (string)
# 61 "lib/grammar.ml"
  )
    | HD
    | FST
    | FIELD
    | EXTENDS
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
  | MenhirState335
  | MenhirState333
  | MenhirState332
  | MenhirState326
  | MenhirState324
  | MenhirState323
  | MenhirState320
  | MenhirState316
  | MenhirState315
  | MenhirState314
  | MenhirState313
  | MenhirState311
  | MenhirState305
  | MenhirState303
  | MenhirState302
  | MenhirState301
  | MenhirState300
  | MenhirState297
  | MenhirState295
  | MenhirState291
  | MenhirState289
  | MenhirState288
  | MenhirState287
  | MenhirState285
  | MenhirState284
  | MenhirState283
  | MenhirState282
  | MenhirState281
  | MenhirState279
  | MenhirState275
  | MenhirState274
  | MenhirState273
  | MenhirState269
  | MenhirState267
  | MenhirState266
  | MenhirState264
  | MenhirState262
  | MenhirState259
  | MenhirState258
  | MenhirState255
  | MenhirState254
  | MenhirState253
  | MenhirState252
  | MenhirState251
  | MenhirState250
  | MenhirState248
  | MenhirState247
  | MenhirState246
  | MenhirState244
  | MenhirState239
  | MenhirState235
  | MenhirState231
  | MenhirState230
  | MenhirState229
  | MenhirState228
  | MenhirState227
  | MenhirState226
  | MenhirState224
  | MenhirState222
  | MenhirState220
  | MenhirState218
  | MenhirState216
  | MenhirState214
  | MenhirState213
  | MenhirState212
  | MenhirState208
  | MenhirState206
  | MenhirState205
  | MenhirState195
  | MenhirState194
  | MenhirState190
  | MenhirState188
  | MenhirState187
  | MenhirState182
  | MenhirState181
  | MenhirState177
  | MenhirState176
  | MenhirState175
  | MenhirState174
  | MenhirState173
  | MenhirState172
  | MenhirState168
  | MenhirState167
  | MenhirState166
  | MenhirState165
  | MenhirState163
  | MenhirState161
  | MenhirState160
  | MenhirState159
  | MenhirState157
  | MenhirState155
  | MenhirState153
  | MenhirState150
  | MenhirState147
  | MenhirState145
  | MenhirState142
  | MenhirState141
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

# 257 "lib/grammar.ml"

let rec _menhir_goto_separated_nonempty_list_COMMA_formal_par_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.texpr option) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState320 | MenhirState63 | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((string * Ast.texpr option) list)) = _v in
        let _v : ((string * Ast.texpr option) list) = 
# 144 "<standard.mly>"
    ( x )
# 269 "lib/grammar.ml"
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
# 280 "lib/grammar.ml"
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
# 302 "lib/grammar.ml"
            ))), _, (xs : ((string * Ast.texpr option) list))) = _menhir_stack in
            let _v : (Ast.abs_mdecl) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 307 "lib/grammar.ml"
             in
            
# 218 "lib/grammar.mly"
         ( MethodAbs(id,retType,params) )
# 312 "lib/grammar.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | METHOD ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | RBRACE ->
                _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack) MenhirState44
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
                    _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | AVG ->
                    _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | BEGIN ->
                    _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | CASET ->
                    _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | CAST ->
                    _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | CONS ->
                    _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | DEBUG ->
                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | DEREF ->
                    _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | EMPTYLIST ->
                    _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | EMPTYPRED ->
                    _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | EMPTYTREE ->
                    _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | FST ->
                    _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | HD ->
                    _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | ID _v ->
                    _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
                | IF ->
                    _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | INSTANCEOF ->
                    _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | INT _v ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
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
    | MenhirState320 ->
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
                    _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | AVG ->
                    _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | BEGIN ->
                    _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | CASET ->
                    _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | CAST ->
                    _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | CONS ->
                    _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | DEBUG ->
                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | DEREF ->
                    _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | EMPTYLIST ->
                    _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | EMPTYPRED ->
                    _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | EMPTYTREE ->
                    _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | FST ->
                    _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | HD ->
                    _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | ID _v ->
                    _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
                | IF ->
                    _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | INSTANCEOF ->
                    _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | INT _v ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
                | ISZERO ->
                    _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | LANGLE ->
                    _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | LBRACE ->
                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | LET ->
                    _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | LETREC ->
                    _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | LIST ->
                    _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | LPAREN ->
                    _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | MAXL ->
                    _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | MIN ->
                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | NEW ->
                    _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | NEWREF ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | NODE ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | PAIR ->
                    _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | PROC ->
                    _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | PROD ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | SELF ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | SEND ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | SET ->
                    _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | SETREF ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | SND ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | SUM ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | SUPER ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | TL ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | UNPAIR ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState323
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState323)
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
    | MenhirState326 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.mdecl))), _, (xs : (Ast.mdecl list))) = _menhir_stack in
        let _v : (Ast.mdecl list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 573 "lib/grammar.ml"
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
# 588 "lib/grammar.ml"
            ))), (id2 : (
# 16 "lib/grammar.mly"
       (string)
# 592 "lib/grammar.ml"
            ))), (id3 : (string option))), _, (ofs : ((string * Ast.texpr option) list))), _, (mths : (Ast.mdecl list))) = _menhir_stack in
            let _v : (Ast.cdecl) = 
# 195 "lib/grammar.mly"
  ( Class(id1,id2,id3,ofs,mths))
# 597 "lib/grammar.ml"
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
# 619 "lib/grammar.ml"
        ))) = _menhir_stack in
        let _v : (string list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 624 "lib/grammar.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (string list)) = _v in
        let _v : (string list) = 
# 144 "<standard.mly>"
    ( x )
# 634 "lib/grammar.ml"
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
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState326
    | RBRACE ->
        _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState326
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState326

and _menhir_goto_nonempty_list_rdecs_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.rdecs) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState255 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * string * Ast.texpr option * Ast.texpr option * Ast.expr))), _, (xs : (Ast.rdecs))) = _menhir_stack in
        let _v : (Ast.rdecs) = 
# 223 "<standard.mly>"
    ( x :: xs )
# 667 "lib/grammar.ml"
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
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState258
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState258)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_field_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.expr) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((string * Ast.expr) list)) = _v in
        let _v : ((string * Ast.expr) list) = 
# 144 "<standard.mly>"
    ( x )
# 785 "lib/grammar.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_field__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState244 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((string * Ast.expr) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string * Ast.expr))) = _menhir_stack in
        let _v : ((string * Ast.expr) list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 796 "lib/grammar.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_field_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.expr list)) = _v in
        let _v : (Ast.expr list) = 
# 144 "<standard.mly>"
    ( x )
# 812 "lib/grammar.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.expr list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Ast.expr))), _) = _menhir_stack in
        let _v : (Ast.expr list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 823 "lib/grammar.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState78 | MenhirState80 | MenhirState297 | MenhirState91 | MenhirState106 | MenhirState110 | MenhirState115 | MenhirState136 | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.expr list)) = _v in
        let _v : (Ast.expr list) = 
# 144 "<standard.mly>"
    ( x )
# 839 "lib/grammar.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.expr list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Ast.expr))), _) = _menhir_stack in
        let _v : (Ast.expr list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 850 "lib/grammar.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run167 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | AVG ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | BEGIN ->
        _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | CASET ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | CAST ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | CONS ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | DEBUG ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | DEREF ->
        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | EMPTYLIST ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | EMPTYPRED ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | EMPTYTREE ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | FST ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | HD ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | ID _v ->
        _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
    | IF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | INSTANCEOF ->
        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState167
    | INT _v ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState167

and _menhir_run172 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | AVG ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | BEGIN ->
        _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | CASET ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | CAST ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | CONS ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | DEBUG ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | DEREF ->
        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | EMPTYLIST ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | EMPTYPRED ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | EMPTYTREE ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | FST ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | HD ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | ID _v ->
        _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
    | IF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | INSTANCEOF ->
        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | INT _v ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v
    | ISZERO ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | LANGLE ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | LBRACE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | LETREC ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | LIST ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | LPAREN ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | MAXL ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | MIN ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | NEW ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | NEWREF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | NODE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | PAIR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | PROC ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | PROD ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | SELF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | SEND ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | SET ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | SETREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | SND ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | SUM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | SUPER ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | TL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | UNPAIR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState172
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172

and _menhir_run176 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | AVG ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | BEGIN ->
        _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | CASET ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | CAST ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | CONS ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | DEBUG ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | DEREF ->
        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | EMPTYLIST ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | EMPTYPRED ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | EMPTYTREE ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | FST ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | HD ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | ID _v ->
        _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
    | IF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | INSTANCEOF ->
        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | INT _v ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
    | ISZERO ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | LANGLE ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | LBRACE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | LETREC ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | LIST ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | LPAREN ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | MAXL ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | MIN ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | NEW ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | NEWREF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | NODE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | PAIR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | PROC ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | PROD ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | SELF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | SEND ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | SET ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | SETREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | SND ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | SUM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | SUPER ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | TL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | UNPAIR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState176
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176

and _menhir_run169 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (id : (
# 16 "lib/grammar.mly"
       (string)
# 1148 "lib/grammar.ml"
        )) = _v in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _) = _menhir_stack in
        let _v : (Ast.expr) = 
# 161 "lib/grammar.mly"
                      ( Proj(e1,id) )
# 1154 "lib/grammar.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run174 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | AVG ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | BEGIN ->
        _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | CASET ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | CAST ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | CONS ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | DEBUG ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | DEREF ->
        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | EMPTYLIST ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | EMPTYPRED ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | EMPTYTREE ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | FST ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | HD ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | ID _v ->
        _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
    | IF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | INSTANCEOF ->
        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | INT _v ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
    | ISZERO ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | LANGLE ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | LBRACE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | LETREC ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | LIST ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | LPAREN ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | MAXL ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | MIN ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | NEW ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | NEWREF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | NODE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | PAIR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | PROC ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | PROD ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | SELF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | SEND ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | SET ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | SETREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | SND ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | SUM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | SUPER ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | TL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | UNPAIR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState174
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState163 ->
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
# 1274 "lib/grammar.ml"
             in
            
# 128 "lib/grammar.mly"
                                                       ( Avg(es) )
# 1279 "lib/grammar.ml"
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
# 1301 "lib/grammar.ml"
             in
            
# 152 "lib/grammar.mly"
                                                    ( Tuple(es) )
# 1306 "lib/grammar.ml"
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
# 1328 "lib/grammar.ml"
             in
            
# 169 "lib/grammar.mly"
                                                        ( List(es))
# 1333 "lib/grammar.ml"
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
# 1355 "lib/grammar.ml"
             in
            
# 129 "lib/grammar.mly"
                                                        ( Maxl(es) )
# 1360 "lib/grammar.ml"
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
# 1381 "lib/grammar.ml"
            ))), _, (xs : (Ast.expr list))) = _menhir_stack in
            let _v : (Ast.expr) = let args = 
# 232 "<standard.mly>"
    ( xs )
# 1386 "lib/grammar.ml"
             in
            
# 163 "lib/grammar.mly"
         ( NewObject(id,args) )
# 1391 "lib/grammar.ml"
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
# 1413 "lib/grammar.ml"
             in
            
# 127 "lib/grammar.mly"
                                                        ( Prod(es) )
# 1418 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState297 ->
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
# 1439 "lib/grammar.ml"
            ))), _, (xs : (Ast.expr list))) = _menhir_stack in
            let _v : (Ast.expr) = let args = 
# 232 "<standard.mly>"
    ( xs )
# 1444 "lib/grammar.ml"
             in
            
# 166 "lib/grammar.mly"
         ( Send(e,id,args) )
# 1449 "lib/grammar.ml"
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
# 1471 "lib/grammar.ml"
             in
            
# 126 "lib/grammar.mly"
                                                       ( Sum(es) )
# 1476 "lib/grammar.ml"
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
# 1497 "lib/grammar.ml"
            ))), _, (xs : (Ast.expr list))) = _menhir_stack in
            let _v : (Ast.expr) = let args = 
# 232 "<standard.mly>"
    ( xs )
# 1502 "lib/grammar.ml"
             in
            
# 168 "lib/grammar.mly"
         ( Super(id,args) )
# 1507 "lib/grammar.ml"
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
# 1529 "lib/grammar.ml"
        ))), _, (t : (Ast.texpr option))) = _menhir_stack in
        let _v : (string * Ast.texpr option) = 
# 221 "lib/grammar.mly"
                                     ( (id, t) )
# 1534 "lib/grammar.ml"
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
# 1558 "lib/grammar.ml"
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
                    _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | AVG ->
                    _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | BEGIN ->
                    _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | CASET ->
                    _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | CAST ->
                    _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | CONS ->
                    _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | DEBUG ->
                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | DEREF ->
                    _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | EMPTYLIST ->
                    _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | EMPTYPRED ->
                    _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | EMPTYTREE ->
                    _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | FST ->
                    _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | HD ->
                    _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | ID _v ->
                    _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
                | IF ->
                    _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | INSTANCEOF ->
                    _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | INT _v ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
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
                _menhir_reduce87 _menhir_env (Obj.magic _menhir_stack) MenhirState121
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
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
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

and _menhir_reduce71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Ast.texpr option) list) = 
# 142 "<standard.mly>"
    ( [] )
# 1814 "lib/grammar.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_formal_par__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "lib/grammar.mly"
       (string)
# 1821 "lib/grammar.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | COMMA | RPAREN ->
        _menhir_reduce87 _menhir_env (Obj.magic _menhir_stack) MenhirState31
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
# 1847 "lib/grammar.ml"
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
# 1858 "lib/grammar.ml"
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

and _menhir_reduce63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.mdecl list) = 
# 211 "<standard.mly>"
    ( [] )
# 1917 "lib/grammar.ml"
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
                _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
        | ARROW | ID _ | TIMES ->
            _menhir_reduce103 _menhir_env (Obj.magic _menhir_stack)
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

and _menhir_reduce87 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.texpr option) = 
# 114 "<standard.mly>"
    ( None )
# 1977 "lib/grammar.ml"
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
# 2009 "lib/grammar.ml"
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
                _menhir_reduce87 _menhir_env (Obj.magic _menhir_stack) MenhirState119
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
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
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
# 2167 "lib/grammar.ml"
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
# 2190 "lib/grammar.ml"
        ))) = _menhir_stack in
        let _v : (string list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2195 "lib/grammar.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_field__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.expr) list) -> 'ttv_return =
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
        let ((_menhir_stack, _menhir_s), _, (xs : ((string * Ast.expr) list))) = _menhir_stack in
        let _v : (Ast.expr) = let fs = 
# 232 "<standard.mly>"
    ( xs )
# 2220 "lib/grammar.ml"
         in
        
# 160 "lib/grammar.mly"
                                                        ( Record(fs) )
# 2225 "lib/grammar.ml"
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
# 2238 "lib/grammar.ml"
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
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
    | MenhirState165 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState166 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 124 "lib/grammar.mly"
                              ( Abs(e) )
# 2369 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166)
    | MenhirState167 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState168
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | DIVIDED | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | MINUS | NEW | NEWREF | NODE | OF | PAIR | PLUS | PROC | PROD | RANGLE | RBRACE | RPAREN | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TIMES | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 122 "lib/grammar.mly"
                              ( Mul(e1,e2) )
# 2391 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168)
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | MINUS | NEW | NEWREF | NODE | OF | PAIR | PLUS | PROC | PROD | RANGLE | RBRACE | RPAREN | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 120 "lib/grammar.mly"
                             ( Add(e1,e2) )
# 2415 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState173)
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | DIVIDED | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | MINUS | NEW | NEWREF | NODE | OF | PAIR | PLUS | PROC | PROD | RANGLE | RBRACE | RPAREN | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TIMES | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 123 "lib/grammar.mly"
                                ( Div(e1,e2) )
# 2435 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175)
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | MINUS | NEW | NEWREF | NODE | OF | PAIR | PLUS | PROC | PROD | RANGLE | RBRACE | RPAREN | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 121 "lib/grammar.mly"
                              ( Sub(e1,e2) )
# 2459 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177)
    | MenhirState78 | MenhirState80 | MenhirState297 | MenhirState91 | MenhirState106 | MenhirState110 | MenhirState115 | MenhirState136 | MenhirState182 | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState181 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState182
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182)
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState181
        | RANGLE | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2580 "lib/grammar.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState181)
    | MenhirState188 | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState187 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188)
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState187
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2701 "lib/grammar.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187)
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | OF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState190 in
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
                            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | AVG ->
                            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | BEGIN ->
                            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | CASET ->
                            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | CAST ->
                            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | CONS ->
                            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | DEBUG ->
                            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | DEREF ->
                            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | EMPTYLIST ->
                            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | EMPTYPRED ->
                            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | EMPTYTREE ->
                            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | FST ->
                            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | HD ->
                            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | ID _v ->
                            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
                        | IF ->
                            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | INSTANCEOF ->
                            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | INT _v ->
                            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
                        | ISZERO ->
                            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | LANGLE ->
                            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | LBRACE ->
                            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | LET ->
                            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | LETREC ->
                            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | LIST ->
                            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | LPAREN ->
                            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | MAXL ->
                            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | MIN ->
                            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | NEW ->
                            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | NEWREF ->
                            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | NODE ->
                            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | PAIR ->
                            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | PROC ->
                            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | PROD ->
                            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | SELF ->
                            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | SEND ->
                            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | SET ->
                            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | SETREF ->
                            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | SND ->
                            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | SUM ->
                            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | SUPER ->
                            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | TL ->
                            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | UNPAIR ->
                            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState194)
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
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState190)
    | MenhirState194 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState195 in
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
                                                    _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | AVG ->
                                                    _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | BEGIN ->
                                                    _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | CASET ->
                                                    _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | CAST ->
                                                    _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | CONS ->
                                                    _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | DEBUG ->
                                                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | DEREF ->
                                                    _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | EMPTYLIST ->
                                                    _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | EMPTYPRED ->
                                                    _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | EMPTYTREE ->
                                                    _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | FST ->
                                                    _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | HD ->
                                                    _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | ID _v ->
                                                    _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
                                                | IF ->
                                                    _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | INSTANCEOF ->
                                                    _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | INT _v ->
                                                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
                                                | ISZERO ->
                                                    _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | LANGLE ->
                                                    _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | LBRACE ->
                                                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | LET ->
                                                    _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | LETREC ->
                                                    _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | LIST ->
                                                    _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | LPAREN ->
                                                    _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | MAXL ->
                                                    _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | MIN ->
                                                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | NEW ->
                                                    _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | NEWREF ->
                                                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | NODE ->
                                                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | PAIR ->
                                                    _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | PROC ->
                                                    _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | PROD ->
                                                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | SELF ->
                                                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | SEND ->
                                                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | SET ->
                                                    _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | SETREF ->
                                                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | SND ->
                                                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | SUM ->
                                                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | SUPER ->
                                                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | TL ->
                                                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | UNPAIR ->
                                                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState205
                                                | _ ->
                                                    assert (not _menhir_env._menhir_error);
                                                    _menhir_env._menhir_error <- true;
                                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205)
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
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState195
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState195)
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState206 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))), _), (id1 : (
# 16 "lib/grammar.mly"
       (string)
# 3088 "lib/grammar.ml"
            ))), (id2 : (
# 16 "lib/grammar.mly"
       (string)
# 3092 "lib/grammar.ml"
            ))), (id3 : (
# 16 "lib/grammar.mly"
       (string)
# 3096 "lib/grammar.ml"
            ))), _, (e3 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 159 "lib/grammar.mly"
                              ( CaseT(e1,e2,id1,id2,id3,e3) )
# 3101 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState206)
    | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState208 in
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
# 3135 "lib/grammar.ml"
                    ))) = _menhir_stack in
                    let _v : (Ast.expr) = 
# 176 "lib/grammar.mly"
                                              ( Cast(e1,id) )
# 3140 "lib/grammar.ml"
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
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState208
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState208)
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState212 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState213
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState213)
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState212
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState212)
    | MenhirState213 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState214 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 174 "lib/grammar.mly"
                                                    ( Cons(e1,e2) )
# 3303 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState214
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState214)
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState216
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState216
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState216
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState216
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState216 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 119 "lib/grammar.mly"
                                ( Debug(e) )
# 3334 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState216
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState216)
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState218 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 140 "lib/grammar.mly"
                                  ( DeRef(e) )
# 3365 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState218
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState218)
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState220 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 173 "lib/grammar.mly"
                                      ( IsEmpty(e) )
# 3396 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState220
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState220)
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState222
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState222
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState222
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState222
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState222 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 131 "lib/grammar.mly"
                              ( Fst(e) )
# 3427 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState222
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState222)
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState224 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 171 "lib/grammar.mly"
                               ( Hd(e) )
# 3458 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState224
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState224)
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState226
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState226
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState226
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState226
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState226 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState227 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState227 _v
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState227
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState227)
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState226
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState226)
    | MenhirState227 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState228
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState228
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState228 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState229 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState229 _v
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState229
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState229)
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState228
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState228
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState228
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState228)
    | MenhirState229 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState230
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | NEW | NEWREF | NODE | OF | PAIR | PROC | PROD | RANGLE | RBRACE | RPAREN | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))), _), _, (e3 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 142 "lib/grammar.mly"
                                                  ( ITE(e1,e2,e3) )
# 3712 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState230)
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState231 in
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
# 3744 "lib/grammar.ml"
                    ))) = _menhir_stack in
                    let _v : (Ast.expr) = 
# 175 "lib/grammar.mly"
                                                  ( IsInstanceOf(e,id) )
# 3749 "lib/grammar.ml"
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
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState231
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState231)
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState235
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState235
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState235
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState235
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState235 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 138 "lib/grammar.mly"
                                   ( IsZero(e) )
# 3800 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState235
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState235)
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState239
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState239
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState239
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState239
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState239
        | RBRACE | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (id : (
# 16 "lib/grammar.mly"
       (string)
# 3829 "lib/grammar.ml"
            ))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (string * Ast.expr) = 
# 187 "lib/grammar.mly"
                          ( (id,e) )
# 3834 "lib/grammar.ml"
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
                    _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState244)
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * Ast.expr))) = _menhir_stack in
                let _v : ((string * Ast.expr) list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3858 "lib/grammar.ml"
                 in
                _menhir_goto_separated_nonempty_list_SEMICOLON_field_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState239)
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState246 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState247 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState247 _v
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState247
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState247)
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState246)
    | MenhirState247 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState248
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState248
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState248
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState248
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState248
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | NEW | NEWREF | NODE | OF | PAIR | PROC | PROD | RANGLE | RBRACE | RPAREN | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, (xs : (string list))), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = let is = 
# 232 "<standard.mly>"
    ( xs )
# 4004 "lib/grammar.ml"
             in
            
# 154 "lib/grammar.mly"
            ( Untuple(is,e1,e2) )
# 4009 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState248)
    | MenhirState250 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState251
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState251
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState251 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState252
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState252
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState252
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState252
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState252
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState252
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState252
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState252
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState252
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState252
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState252
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState252
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState252
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState252
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState252
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
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
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState251
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState251
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState251
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
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState253
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState253
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState253
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState253
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState253
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | NEW | NEWREF | NODE | OF | PAIR | PROC | PROD | RANGLE | RBRACE | RPAREN | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (x : (
# 16 "lib/grammar.mly"
       (string)
# 4148 "lib/grammar.ml"
            ))), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 133 "lib/grammar.mly"
                                                ( Let(x,e1,e2) )
# 4153 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState253)
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState254
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState254
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState254
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState254
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState254
        | ID _ | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (x : (
# 16 "lib/grammar.mly"
       (string)
# 4180 "lib/grammar.ml"
            ))), (y : (
# 16 "lib/grammar.mly"
       (string)
# 4184 "lib/grammar.ml"
            ))), _, (targ : (Ast.texpr option))), _, (tres : (Ast.texpr option))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (string * string * Ast.texpr option * Ast.texpr option * Ast.expr) = 
# 181 "lib/grammar.mly"
            ( (x, y, targ, tres, e) )
# 4189 "lib/grammar.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState255 _v
            | IN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * string * Ast.texpr option * Ast.texpr option * Ast.expr))) = _menhir_stack in
                let _v : (Ast.rdecs) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 4204 "lib/grammar.ml"
                 in
                _menhir_goto_nonempty_list_rdecs_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState255)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState254)
    | MenhirState258 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState259
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | NEW | NEWREF | NODE | OF | PAIR | PROC | PROD | RANGLE | RBRACE | RPAREN | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (decs : (Ast.rdecs))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 134 "lib/grammar.mly"
                                                    ( Letrec(decs, e) )
# 4236 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState259)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState262 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 147 "lib/grammar.mly"
                                  ( Sub(Int 0, e) )
# 4265 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState262)
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ABS ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState264 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState266
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
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
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
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
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState264 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 145 "lib/grammar.mly"
                           (e)
# 4453 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
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
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState264)
    | MenhirState266 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState267
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState267
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState267
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState267
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState267 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 149 "lib/grammar.mly"
                                              ( Pair(e1,e2) )
# 4502 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState267
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState267)
    | MenhirState264 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState269
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState269
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState269
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState269
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState269 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 137 "lib/grammar.mly"
                                       ( App(e1,e2) )
# 4533 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState269
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState269)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState273 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState274 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState274 _v
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState274
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState274)
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState273
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState273)
    | MenhirState274 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState275
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState275
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState275
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState275
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState275 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 125 "lib/grammar.mly"
                                               ( Min(e1,e2) )
# 4676 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState275
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState275)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState279
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState279
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState279
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState279
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState279 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 139 "lib/grammar.mly"
                                   ( NewRef(e) )
# 4707 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState279
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState279)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState281 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState282 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState282 _v
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState282
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState282)
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState281
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState281
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState281
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState281
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState281
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState281)
    | MenhirState282 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState283 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState284 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState284 _v
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState284
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState284)
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState283)
    | MenhirState284 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState285
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState285
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState285
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState285
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState285 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))), _), _, (e3 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 156 "lib/grammar.mly"
                                                                  ( Node(e1,e2,e3) )
# 4962 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState285
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState285)
    | MenhirState99 ->
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
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState288 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState288
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState288 _v
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
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState287
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState287
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState287
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState287
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState287
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
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState289
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState289
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState289
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState289
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState289 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 130 "lib/grammar.mly"
                                                ( Pair(e1,e2) )
# 5105 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState289
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState289)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState291
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState291
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState291
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState291
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState291 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (x : (
# 16 "lib/grammar.mly"
       (string)
# 5135 "lib/grammar.ml"
            ))), _, (t : (Ast.texpr option))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 136 "lib/grammar.mly"
         ( Proc(x,t,e) )
# 5140 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState291
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState291)
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState295
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState295
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState295 in
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
                    _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | AVG ->
                    _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | BEGIN ->
                    _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | CASET ->
                    _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | CAST ->
                    _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | CONS ->
                    _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | DEBUG ->
                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | DEREF ->
                    _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | EMPTYLIST ->
                    _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | EMPTYPRED ->
                    _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | EMPTYTREE ->
                    _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | FST ->
                    _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | HD ->
                    _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | ID _v ->
                    _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
                | IF ->
                    _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | INSTANCEOF ->
                    _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | INT _v ->
                    _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
                | ISZERO ->
                    _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | LANGLE ->
                    _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | LBRACE ->
                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | LET ->
                    _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | LETREC ->
                    _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | LIST ->
                    _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | LPAREN ->
                    _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | MAXL ->
                    _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | MIN ->
                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | NEW ->
                    _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | NEWREF ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | NODE ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | PAIR ->
                    _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | PROC ->
                    _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | PROD ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | SELF ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | SEND ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | SET ->
                    _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | SETREF ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | SND ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | SUM ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | SUPER ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | TL ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | UNPAIR ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | RPAREN ->
                    _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState297
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState297)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState295
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState295
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState295
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState295)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | NEW | NEWREF | NODE | OF | PAIR | PROC | PROD | RANGLE | RBRACE | RPAREN | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (x : (
# 16 "lib/grammar.mly"
       (string)
# 5294 "lib/grammar.ml"
            ))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 143 "lib/grammar.mly"
                                ( Set(x,e) )
# 5299 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState300)
    | MenhirState84 ->
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
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState302
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _v
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
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState301
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState301
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState301
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState301
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState301
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
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState303
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState303
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState303
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState303
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState303 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 141 "lib/grammar.mly"
                                                      ( SetRef(e1,e2) )
# 5440 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState303
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState303)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState305
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState305
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState305
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState305
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState305 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 132 "lib/grammar.mly"
                              ( Snd(e) )
# 5471 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState305
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState305)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState311
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState311
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState311
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState311
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState311 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 172 "lib/grammar.mly"
                               ( Tl(e) )
# 5502 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState311
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState311)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState313
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState313
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState313 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ABS ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState314 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState314 _v
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState314
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState314)
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState313
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState313
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState313
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState313)
    | MenhirState314 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState315
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState315
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState315
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState315
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState315
        | ABS | AVG | BEGIN | CASET | CAST | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYPRED | EMPTYTREE | END | EOF | FST | HD | ID _ | IF | IN | INSTANCEOF | INT _ | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | NEW | NEWREF | NODE | OF | PAIR | PROC | PROD | RANGLE | RBRACE | RPAREN | SELF | SEMICOLON | SEND | SET | SETREF | SND | SUM | SUPER | THEN | TL | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (x : (
# 16 "lib/grammar.mly"
       (string)
# 5643 "lib/grammar.ml"
            ))), (y : (
# 16 "lib/grammar.mly"
       (string)
# 5647 "lib/grammar.ml"
            ))), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 151 "lib/grammar.mly"
                ( Unpair(x,y,e1,e2) )
# 5652 "lib/grammar.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState315)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState316 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (id : (
# 16 "lib/grammar.mly"
       (string)
# 5680 "lib/grammar.ml"
            ))), _, (xs : ((string * Ast.texpr option) list))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.mdecl) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 5685 "lib/grammar.ml"
             in
            
# 210 "lib/grammar.mly"
         ( Method(id,None,params,e) )
# 5690 "lib/grammar.ml"
             in
            _menhir_goto_method_decl _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState316
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState316)
    | MenhirState323 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState324 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, (retType : (Ast.texpr))), (id : (
# 16 "lib/grammar.mly"
       (string)
# 5720 "lib/grammar.ml"
            ))), _, (xs : ((string * Ast.texpr option) list))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.mdecl) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 5725 "lib/grammar.ml"
             in
            
# 214 "lib/grammar.mly"
       ( Method(id,Some retType,params,e) )
# 5730 "lib/grammar.ml"
             in
            _menhir_goto_method_decl _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState324
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState324)
    | MenhirState332 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run174 _menhir_env (Obj.magic _menhir_stack) MenhirState333
        | DOT ->
            _menhir_run169 _menhir_env (Obj.magic _menhir_stack) MenhirState333
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState333 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (cls : (Ast.cdecl list))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.prog) = 
# 112 "lib/grammar.mly"
                                                  ( AProg(cls,e) )
# 5756 "lib/grammar.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.prog)) = _v in
            Obj.magic _1
        | MINUS ->
            _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState333
        | PLUS ->
            _menhir_run172 _menhir_env (Obj.magic _menhir_stack) MenhirState333
        | TIMES ->
            _menhir_run167 _menhir_env (Obj.magic _menhir_stack) MenhirState333
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState333)
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
# 5790 "lib/grammar.ml"
         in
        
# 144 "lib/grammar.mly"
                                                   ( BeginEnd(es) )
# 5795 "lib/grammar.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expr list) = 
# 142 "<standard.mly>"
    ( [] )
# 5810 "lib/grammar.ml"
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
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState335
    | INTERFACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState335
    | ABS | AVG | BEGIN | CASET | CAST | CONS | DEBUG | DEREF | EMPTYLIST | EMPTYPRED | EMPTYTREE | FST | HD | ID _ | IF | INSTANCEOF | INT _ | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | NEW | NEWREF | NODE | PAIR | PROC | PROD | SELF | SEND | SET | SETREF | SND | SUM | SUPER | TL | UNPAIR ->
        _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState335
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState335

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
# 5847 "lib/grammar.ml"
         in
        
# 232 "lib/grammar.mly"
                                                            ( RecordType(ts) )
# 5852 "lib/grammar.ml"
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
# 5865 "lib/grammar.ml"
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
# 5920 "lib/grammar.ml"
            ))), _, (t : (Ast.texpr))) = _menhir_stack in
            let _v : (string * Ast.texpr) = 
# 190 "lib/grammar.mly"
                          ( (id,t) )
# 5925 "lib/grammar.ml"
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
# 5949 "lib/grammar.ml"
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
# 229 "lib/grammar.mly"
                                ( PairType(t1,t2) )
# 5971 "lib/grammar.ml"
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
# 228 "lib/grammar.mly"
                               ( FuncType(t1,t2) )
# 5989 "lib/grammar.ml"
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
# 230 "lib/grammar.mly"
                             ( t1 )
# 6013 "lib/grammar.ml"
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
# 231 "lib/grammar.mly"
                    ( RefType(t1) )
# 6031 "lib/grammar.ml"
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
                    _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState30
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
# 184 "lib/grammar.mly"
                 ( t )
# 6089 "lib/grammar.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (x : (Ast.texpr)) = _v in
            let _v : (Ast.texpr option) = 
# 116 "<standard.mly>"
    ( Some x )
# 6097 "lib/grammar.ml"
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
# 6120 "lib/grammar.ml"
            )) = _v in
            let ((_menhir_stack, _menhir_s), _, (t : (Ast.texpr))) = _menhir_stack in
            let _v : (string * Ast.texpr option) = 
# 204 "lib/grammar.mly"
                        ( (id,Some t) )
# 6126 "lib/grammar.ml"
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
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState320 _v
                | RPAREN ->
                    _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState320
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState320)
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
# 6191 "lib/grammar.ml"
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
            _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState60
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
        _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_reduce103 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 16 "lib/grammar.mly"
       (string)
# 6229 "lib/grammar.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id : (
# 16 "lib/grammar.mly"
       (string)
# 6235 "lib/grammar.ml"
    ))) = _menhir_stack in
    let _v : (Ast.texpr) = 
# 224 "lib/grammar.mly"
        ( UserType(id) )
# 6240 "lib/grammar.ml"
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
                                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | AVG ->
                                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | BEGIN ->
                                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | CASET ->
                                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | CAST ->
                                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | CONS ->
                                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | DEBUG ->
                                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | DEREF ->
                                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | EMPTYLIST ->
                                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | EMPTYPRED ->
                                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | EMPTYTREE ->
                                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | FST ->
                                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | HD ->
                                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | ID _v ->
                                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
                            | IF ->
                                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | INSTANCEOF ->
                                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                            | INT _v ->
                                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
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
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
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
                _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState78
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
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
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState80
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
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
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
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
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | AVG ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | BEGIN ->
        _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | CASET ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | CAST ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | CONS ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | DEBUG ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | DEREF ->
        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | EMPTYLIST ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | EMPTYPRED ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | EMPTYTREE ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | FST ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | HD ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | ID _v ->
        _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | IF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | INSTANCEOF ->
        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | INT _v ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
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
# 164 "lib/grammar.mly"
        ( Self )
# 7162 "lib/grammar.ml"
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
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
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState91
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
                _menhir_reduce87 _menhir_env (Obj.magic _menhir_stack) MenhirState94
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
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
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState106
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
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
                _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState106
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
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
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState110
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
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | AVG ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | BEGIN ->
        _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | CASET ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | CAST ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | CONS ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | DEBUG ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | DEREF ->
        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | EMPTYLIST ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | EMPTYPRED ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | EMPTYTREE ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | FST ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | HD ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | ID _v ->
        _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | IF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | INSTANCEOF ->
        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | INT _v ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
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
# 148 "lib/grammar.mly"
                 ( Unit )
# 8122 "lib/grammar.ml"
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
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
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState115
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
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | AVG ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | CASET ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | CAST ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | CONS ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | DEBUG ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | DEREF ->
                _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | EMPTYLIST ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | EMPTYPRED ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | EMPTYTREE ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | FST ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | HD ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | ID _v ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
            | IF ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | INSTANCEOF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | INT _v ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
            | ISZERO ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | LANGLE ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | LBRACE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | LETREC ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | LIST ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | LPAREN ->
                _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | MAXL ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | MIN ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | NEW ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | NEWREF ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | NODE ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | PAIR ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | PROC ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | PROD ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | SELF ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | SEND ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | SET ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | SETREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | SND ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | SUM ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | SUPER ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | TL ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | UNPAIR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState250)
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
# 8389 "lib/grammar.ml"
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
        let _v : ((string * Ast.expr) list) = 
# 142 "<standard.mly>"
    ( [] )
# 8417 "lib/grammar.ml"
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
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | AVG ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | BEGIN ->
        _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | CASET ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | CAST ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | CONS ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | DEBUG ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | DEREF ->
        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | EMPTYLIST ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | EMPTYPRED ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | EMPTYTREE ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | FST ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | HD ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | ID _v ->
        _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | IF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | INSTANCEOF ->
        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState136
    | INT _v ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
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
        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState136
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
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

and _menhir_run139 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 15 "lib/grammar.mly"
       (int)
# 8627 "lib/grammar.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 15 "lib/grammar.mly"
       (int)
# 8635 "lib/grammar.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 117 "lib/grammar.mly"
          ( Int i )
# 8640 "lib/grammar.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run140 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run142 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | AVG ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | BEGIN ->
        _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | CASET ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | CAST ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | CONS ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | DEBUG ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | DEREF ->
        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | EMPTYLIST ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | EMPTYPRED ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | EMPTYTREE ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | FST ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | HD ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | ID _v ->
        _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | IF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | INSTANCEOF ->
        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | INT _v ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | ISZERO ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | LANGLE ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | LBRACE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | LETREC ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | LIST ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | LPAREN ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | MAXL ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | MIN ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | NEW ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | NEWREF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | NODE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | PAIR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | PROC ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | PROD ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | SELF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | SEND ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | SET ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | SETREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | SND ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | SUM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | SUPER ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | TL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | UNPAIR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142

and _menhir_run143 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "lib/grammar.mly"
       (string)
# 8844 "lib/grammar.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (
# 16 "lib/grammar.mly"
       (string)
# 8852 "lib/grammar.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 118 "lib/grammar.mly"
         ( Var x )
# 8857 "lib/grammar.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run144 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
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
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.expr) = 
# 155 "lib/grammar.mly"
            ( EmptyTree )
# 9076 "lib/grammar.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run149 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run151 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.expr) = 
# 170 "lib/grammar.mly"
            ( EmptyList )
# 9191 "lib/grammar.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run152 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState157
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState159
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
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
    | ABS ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | AVG ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | BEGIN ->
        _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | CASET ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | CAST ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | CONS ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | DEBUG ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | DEREF ->
        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | EMPTYLIST ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | EMPTYPRED ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | EMPTYTREE ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | FST ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | HD ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | ID _v ->
        _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
    | IF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | INSTANCEOF ->
        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | INT _v ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
    | ISZERO ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | LANGLE ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | LBRACE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | LETREC ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | LIST ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | LPAREN ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | MAXL ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | MIN ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | NEW ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | NEWREF ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | NODE ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | PAIR ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | PROC ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | PROD ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | SELF ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | SEND ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | SET ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | SETREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | SND ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | SUM ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | SUPER ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | TL ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | UNPAIR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState160
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160

and _menhir_run161 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ABS ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | AVG ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | BEGIN ->
        _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | CASET ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | CAST ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | CONS ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | DEBUG ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | DEREF ->
        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | EMPTYLIST ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | EMPTYPRED ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | EMPTYTREE ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | FST ->
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | HD ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | ID _v ->
        _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
    | IF ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | INSTANCEOF ->
        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState161
    | INT _v ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
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
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState161 in
        let _v : (Ast.expr list) = 
# 142 "<standard.mly>"
    ( [] )
# 9798 "lib/grammar.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161

and _menhir_run162 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
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
        | RPAREN ->
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState165
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
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
# 10032 "lib/grammar.ml"
            ))), _, (amths : (Ast.abs_mdecl list))) = _menhir_stack in
            let _v : (Ast.cdecl) = 
# 197 "lib/grammar.mly"
  ( Interface(id,amths))
# 10037 "lib/grammar.ml"
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
# 10053 "lib/grammar.ml"
         in
        _menhir_goto_list_abstract_method_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.texpr) = 
# 227 "lib/grammar.mly"
         ( UnitType )
# 10066 "lib/grammar.ml"
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
# 10134 "lib/grammar.ml"
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
# 225 "lib/grammar.mly"
        ( IntType )
# 10149 "lib/grammar.ml"
     in
    _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "lib/grammar.mly"
       (string)
# 10156 "lib/grammar.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce103 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.texpr) = 
# 226 "lib/grammar.mly"
         ( BoolType )
# 10170 "lib/grammar.ml"
     in
    _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Ast.texpr option) list) = 
# 211 "<standard.mly>"
    ( [] )
# 10179 "lib/grammar.ml"
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
            _menhir_reduce103 _menhir_env (Obj.magic _menhir_stack)
        | FIELD | METHOD | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (id : (
# 16 "lib/grammar.mly"
       (string)
# 10205 "lib/grammar.ml"
            ))) = _menhir_stack in
            let _v : (string * Ast.texpr option) = 
# 203 "lib/grammar.mly"
               ( (id,None) )
# 10210 "lib/grammar.ml"
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
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | AVG ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | CASET ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | CAST ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | CONS ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | DEBUG ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | DEREF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | EMPTYLIST ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | EMPTYPRED ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | EMPTYTREE ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | FST ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | HD ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | ID _v ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState332 _v
        | IF ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | INSTANCEOF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | INT _v ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState332 _v
        | ISZERO ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | LANGLE ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | LBRACE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | LET ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | LETREC ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | LIST ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | LPAREN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | MAXL ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | MIN ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | NEW ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | NEWREF ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | NODE ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | PAIR ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | PROC ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | PROD ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | SELF ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | SEND ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | SET ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | SETREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | SND ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | SUM ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | SUPER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | TL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | UNPAIR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState332
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState332)
    | MenhirState335 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.cdecl))), _, (xs : (Ast.cdecl list))) = _menhir_stack in
        let _v : (Ast.cdecl list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 10336 "lib/grammar.ml"
         in
        _menhir_goto_list_iface_or_class_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.abs_mdecl list) = 
# 211 "<standard.mly>"
    ( [] )
# 10347 "lib/grammar.ml"
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
            _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState53
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
    | MenhirState335 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState333 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState332 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState326 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState324 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState323 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState320 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState316 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState315 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState314 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState313 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState311 ->
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
    | MenhirState300 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState297 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState295 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState291 ->
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
    | MenhirState285 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState284 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState283 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState282 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState281 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState279 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState275 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState274 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState273 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState269 ->
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
    | MenhirState264 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState262 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState259 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState258 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState255 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState254 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState253 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState252 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState251 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState250 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState248 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState247 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState246 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState244 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState239 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState235 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState231 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState230 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState229 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState228 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState227 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState226 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState224 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState222 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState220 ->
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
    | MenhirState214 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState213 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState212 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState208 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState206 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState195 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState194 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState175 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState172 ->
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
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState165 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState160 ->
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
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
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

and _menhir_reduce61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.cdecl list) = 
# 211 "<standard.mly>"
    ( [] )
# 11011 "lib/grammar.ml"
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
                _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack) MenhirState3
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
# 11088 "lib/grammar.ml"
                        )) = _v in
                        let _v : (string) = 
# 200 "lib/grammar.mly"
                    ( id )
# 11093 "lib/grammar.ml"
                         in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (x : (string)) = _v in
                        let _v : (string option) = 
# 116 "<standard.mly>"
    ( Some x )
# 11101 "lib/grammar.ml"
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
# 11114 "lib/grammar.ml"
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
    | ABS | AVG | BEGIN | CASET | CAST | CONS | DEBUG | DEREF | EMPTYLIST | EMPTYPRED | EMPTYTREE | FST | HD | ID _ | IF | INSTANCEOF | INT _ | ISZERO | LANGLE | LBRACE | LET | LETREC | LIST | LPAREN | MAXL | MIN | NEW | NEWREF | NODE | PAIR | PROC | PROD | SELF | SEND | SET | SETREF | SND | SUM | SUPER | TL | UNPAIR ->
        _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 11180 "lib/grammar.ml"
