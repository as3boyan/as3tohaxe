/*
 * Copyright (c) 2008-2011, Nicolas Cannasse
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package as3hx;
import as3hx.As3;
import haxe.ds.GenericStack;
import haxe.ds.GenericStack;
import haxe.ds.StringMap;
import haxe.ds.StringMap;

enum Error {
	EInvalidChar( c : Int );
	EUnexpected( s : String );
	EUnterminatedString;
	EUnterminatedComment;
	EUnterminatedXML;
}

enum Token {
	TEof;
	TConst( c : Const );
	TId( s : String );
	TOp( s : String );
	TPOpen;
	TPClose;
	TBrOpen;
	TBrClose;
	TDot;
	TComma;
	TSemicolon;
	TBkOpen;
	TBkClose;
	TQuestion;
	TDoubleDot;
	TAt;
	TNs;
	TComment( s : String, isBlock : Bool );
}

/**
 * ...
 * @author Nicolas Cannasse
 * @author Russell Weir
 */
class Parser {

	// config / variables
	public var line : Int;
	public var identChars : String;
	//public var opPriority : Hash<Int>;
	public var opPriority : StringMap<Int>;
	public var unopsPrefix : Array<String>;
	public var unopsSuffix : Array<String>;

	// implementation
	var input : haxe.io.Input;
	var char : Int;
	var ops : Array<Bool>;
	var idents : Array<Bool>;
	//var tokens : haxe.FastList<Token>;
	var tokens : GenericStack<Token>;
	var no_comments : Bool;

	public function new() {
		line = 1;
		identChars = "$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_";
		var p = [
			["%", "*", "/"],
			["+", "-"],
			["<<", ">>", ">>>"],
			[">", "<", ">=", "<="],
			["==", "!="],
			["&"],
			["^"],
			["|"],
			["&&"],
			["||"],
			["?:"],
			["=", "+=", "-=", "*=", "%=", "/=", "<<=", ">>=", ">>>=", "&=", "^=", "|=", "&&=", "||="]
		];
		opPriority = new StringMap();
		for( i in 0...p.length )
			for( op in p[i] )
				opPriority.set(op, i);
		unopsPrefix = ["!", "++", "--", "-", "+", "~"];
		for( op in unopsPrefix )
			if( !opPriority.exists(op) )
				opPriority.set(op, -1);
		unopsSuffix = ["++", "--"];
		no_comments = false;
	}

	public function parseString( s : String ) {
		line = 1;
		return parse( new haxe.io.StringInput(s) );
	}

	public function parse( s : haxe.io.Input ) {
		char = 0;
		input = s;
		ops = new Array();
		idents = new Array();
		tokens = new GenericStack<Token>();
		for( op in opPriority.keys() )
			for( i in 0...op.length )
				ops[op.charCodeAt(i)] = true;
		for( i in 0...identChars.length )
			idents[identChars.charCodeAt(i)] = true;
		return parseProgram();
	}
	
	inline function add(tk) {
		tokens.add(tk);
	}

	function opt(tk,ncmnt=false) {
		var f = function() {
			var t = token();
			if( Type.enumEq(t, tk) )
				return true;
			add(t);
			return false;
		}
		return ncmnt ? ignoreComments(f) : f();
	}
	
	function ensure(tk) {
		ignoreComments(function() {
			var t = token();
			if( !Type.enumEq(t, tk) )
				unexpected(t);
			return null;
		});
	}

	function ignoreComments(f:Dynamic) : Dynamic {
		var old = no_comments;
		no_comments = true;
		var rv = f();
		no_comments = old;
		return rv;
	}
	
	function parseProgram() : Program {
		#if debug trace("parseProgram()"); #end
		var pack = [];
		var header:Array<Expr> = [];
		while(true) {
			var t = token();
			switch(t) {
			case TId(s):
				if( s != "package" )
					unexpected(t);
				if( opt(TBrOpen) )
					pack = []
				else {
					pack = parsePackage();
					ensure(TBrOpen);
				}
				break;
			case TComment(s,b):
				header.push(EComment(s,b));
			default:
				unexpected(t);
			}
		}
		var imports = [];
		var defs = [];
		var closed = false;
		while( true ) {
			var tk = token();
			switch( tk ) {
			case TBrClose:
				if( !closed ) {
					closed = true;
					continue;
				}
			case TEof:
				if( closed )
					break;
			case TBkOpen:
				add(tk);
				defs.push(parseDefinition());
				continue;
			case TId(id):
				switch( id ) {
				case "import":
					imports.push(parseImport());
					end();
					continue;
				case "use":
					parseUse();
					continue;
				case "public", "class", "internal", "interface", "dynamic", "function":
					add(tk);
					defs.push(parseDefinition());
					continue;
				default:
				}
			case TSemicolon:
				continue;
			case TComment(s,b):
				header.push(EComment(s,b));
				continue;
			default:
			}
			unexpected(tk);
		}
		if( !closed )
			unexpected(TEof);
		return {
			header : header,
			pack : pack,
			imports : imports,
			defs : defs,
		};
	}
	
	function parseUse() {
		ensure(TId("namespace"));
		var ns = this.id();
		end();
	}
	
	function parseImport() {
		#if debug trace("parseImport()"); #end
		var a = [id()];
		while( true ) {
			var tk = token();
			switch( tk ) {
			case TDot:
				tk = token();
				switch(tk) {
				case TId(id): a.push(id);
				case TOp(op):
					if( op == "*" ) {
						a.push(op);
						break;
					}
					unexpected(tk);
				default: unexpected(tk);
				}
			default:
				add(tk);
				break;
			}
		}
		return a;
	}
	
	function parseMetadata() {
		#if debug trace("parseMetadata()"); #end
		var ml = [];
		while( opt(TBkOpen) ) {
			var name = id();
			var args = [];
			if( opt(TPOpen) )
				while( !opt(TPClose) ) {
					var n = null;
					switch(peek()) {
					case TId(i):
						n = id();
						ensure(TOp("="));
					case TConst(_):
					default:
						unexpected(peek());
					}
					var e = parseExpr();
					args.push( { name : n, val :e } );
					opt(TComma);
				}
			ensure(TBkClose);
			ml.push( { name : name, args : args } );
		}
		return ml;
	}
	
	function parseDefinition() {
		#if debug trace("parseDefinition()"); #end
		var kwds = [];
		var meta = parseMetadata();
		while( true ) {
			var id = id();
			switch( id ) {
			case "public", "internal", "final", "dynamic": kwds.push(id);
			case "use":
				parseUse();
				continue;
			case "class":
				return CDef(parseClass(kwds,meta));
			case "interface":
				var c = parseClass(kwds, meta);
				c.isInterface = true;
				return CDef(c);
			case "function":
				return FDef(parseFunDef(kwds, meta));
			case "namespace":
				return NDef(parseNsDef(kwds, meta));
			default: unexpected(TId(id));
			}
		}
		return null;
	}
	
	function parseFunDef(kwds, meta) : FunctionDef {
		#if debug trace("parseFunDef()"); #end
		var fname = id();
		var f = parseFun();
		return {
			kwds : kwds,
			meta : meta,
			name : fname,
			f : f,
		};
	}
	
	function parseNsDef(kwds, meta) : NamespaceDef {
		#if debug trace("parseNsDef()"); #end
		var name = id();
		var value = null;
		if( opt(TOp("=")) ) {
			var t = token();
			value = switch( t ) {
			case TConst(c):
				switch( c ) {
				case CString(str): str;
				default: unexpected(t);
				}
			default:
				unexpected(t);
			};
		}
		return {
			kwds : kwds,
			meta : meta,
			name : name,
			value : value,
		};
	}
	
	function parseClass(kwds,meta) : ClassDef {
		var cname = id();
		#if debug trace("parseClass("+cname+")"); #end
		var fields = new Array();
		var impl = [], extend = null, inits = [];
		while( true ) {
			if( opt(TId("implements")) ) {
				impl.push(parseType());
				while( opt(TComma) )
					impl.push(parseType());
				continue;
			}
			if( opt(TId("extends")) ) {
				extend = parseType();
				continue;
			}
			break;
		}
		ensure(TBrOpen);
		while( true ) {
			if( opt(TBrClose) ) break;
			var meta = parseMetadata();
			var kwds = [];
			var comments = [];
			while( true )  {
				var t = token();
				switch( t ) {
				case TId(id):
					switch( id ) {
					case "public", "static", "private", "protected", "override", "internal", "final": kwds.push(id);
					case "const":
						kwds.push(id);
						do {
							fields.push(parseClassVar(kwds, meta, comments));
							meta = [];
							comments = [];
						} while( opt(TComma) );
						end();
						break;
					case "var":
						do {
							fields.push(parseClassVar(kwds, meta, []));
							meta = [];
							comments = [];
						} while( opt(TComma) );
						end();
						break;
					case "function":
						fields.push(parseClassFun(kwds, meta, []));
						comments = [];
						break;
					case "use":
						parseUse();
						break;
					default:
						kwds.push(id);
					}
				case TComment(s,b):
					fields.push({name:null, meta:null, kwds:[], kind:FComment, comments:[EComment(s,b)]});
					break;
				default:
					add(t);
					while( kwds.length > 0 )
						add(TId(kwds.pop()));
					inits.push(parseExpr());
					end();
					break;
				}
			}
		}
		#if debug trace("parseClass("+cname+") finished"); #end
		return {
			kwds : kwds,
			isInterface : false,
			meta : meta,
			name : cname,
			fields : fields,
			implement : impl,
			extend : extend,
			inits : inits,
		};
	}
	
	function parseType() {
		#if debug trace("parseType()"); #end
		// this is a ugly hack in order to fix lexer issue with "var x:*=0"
		var tmp = opPriority.get("*=");
		opPriority.remove("*=");
		if( opt(TOp("*")) ) {
			opPriority.set("*=",tmp);
			return TStar;
		}
		opPriority.set("*=",tmp);

		// hack for _i = new (obj as Class)() as DisplayObject;
		var tc : T  = cast ignoreComments(
			function () : Dynamic {
				var t = peek();
				return switch(t) {
				case TPOpen: TComplex(parseExpr());
				default: null;
				}
			}
		);
		if(tc != null) return tc;
		var t = id();
		if( t == "Vector" ) {
			ensure(TDot);
			ensure(TOp("<"));
			var t = parseType();
			switch( peek() ) {
			case TOp(s):
				token();
				var tl = [];
				while( s.charAt(0) == ">" ) {
					tl.unshift(">");
					s = s.substr(1);
				}
				if( s.length > 0 )
					tl.unshift(s);
				for( op in tl )
					add(TOp(op));
			default:
			}
			ensure(TOp(">"));
			return TVector(t);
		}
		var a = [t];
		while( true ) {
			var tk = token();
			switch( tk ) {
			case TDot:
				tk = token();
				switch(tk) {
				case TId(id): a.push(id);
				default: unexpected(tk);
				}
			case TComment(s,b):
			default:
				add(tk);
				break;
			}
		}
		return TPath(a);
	}

	function parseClassVar(kwds,meta,comments) : ClassField {
		#if debug trace("parseClassVar()"); #end
		var name = id();
		var t = null, val = null;
		if( opt(TDoubleDot) )
			t = parseType();
		if( opt(TOp("=")) )
			val = parseExpr();
		return {
			comments : [],
			kwds : kwds,
			meta : meta,
			name : name,
			kind : FVar(t, val),
		};
	}
	
	function parseClassFun(kwds:Array<String>,meta,comments) : ClassField {
		#if debug trace("parseClassFun()"); #end
		var name = id();
		if( name == "get" || name == "set" ) {
			kwds.push(name);
			name = id();
		}
		var f = parseFun();
		end();
		return {
			comments : comments,
			kwds : kwds,
			meta : meta,
			name : name,
			kind : FFun(f),
		};
	}
	
	function parseFun() : Function {
		#if debug trace("parseFun()"); #end
		var f = {
			args : [],
			varArgs : null,
			ret : null,
			expr : null,
		};
		ensure(TPOpen);
		if( !opt(TPClose) )
			while( true ) {
				if( opt(TDot) ) {
					ensure(TDot);
					ensure(TDot);
					f.varArgs = id();
					if( opt(TDoubleDot) )
						ensure(TId("Array"));
					ensure(TPClose);
					break;
				}
				var name = id(), t = null, val = null;
				if( opt(TDoubleDot) )
					t = parseType();
				if( opt(TOp("=")) )
					val = parseExpr();
				f.args.push( { name : name, t : t, val : val } );
				if( opt(TPClose) )
					break;
				ensure(TComma);
			}
		if( opt(TDoubleDot) )
			f.ret = parseType();
		if( peek() == TBrOpen ) {
			f.expr = parseExpr(true);
			switch(f.expr) {
			case EObject(fl):
				if(fl.length == 0) {
					f.expr = EBlock([]);
				} else {
					throw "unexpected " + Std.string(f.expr);
				}
			case EBlock(_):
			default:
				throw "unexpected " + Std.string(f.expr);
			}
		}
		return f;
	}
	
	function parsePackage() {
		#if debug trace("parsePackage()"); #end
		var a = [id()];
		while( true ) {
			var tk = token();
			switch( tk ) {
			case TDot:
				tk = token();
				switch(tk) {
				case TId(id): a.push(id);
				default: unexpected(tk);
				}
			default:
				add(tk);
				break;
			}
		}
		return a;
	}

	function unexpected( tk ) : Dynamic {
		throw EUnexpected(tokenString(tk));
		return null;
	}

	function end() {
		while( opt(TSemicolon) ) {
		}
	}
	
	function parseFullExpr() {
		#if debug trace("parseFullExpr()"); #end
		var e = parseExpr();
		if( opt(TDoubleDot) ) {
			switch( e ) {
			case EIdent(l): e = ELabel(l);
			default: add(TDoubleDot);
			}
		}
		if( !opt(TComma) )
			end();
		return e;
	}

	function parseObject() {
		#if debug trace("parseObject"); #end
		// parse object
		var fl = new Array();
		ignoreComments( function() {
			while( true ) {
				var tk = token();
				var id = null;
				switch( tk ) {
				case TId(i): id = i;
				case TConst(c):
					switch( c ) {
					case CInt(v): if( v.charCodeAt(1) == "x".code ) id = Std.string(Std.parseInt(v)) else id = v;
					case CFloat(f): id = f;
					case CString(s): id = s;
					}
				case TBrClose:
					break;
				default:
					unexpected(tk);
				}
				ensure(TDoubleDot);
				fl.push({ name : id, e : parseExpr() });
				tk = token();
				switch( tk ) {
				case TBrClose:
					break;
				case TComma:
				default:
					unexpected(tk);
				}
			}
		});
		return parseExprNext(EObject(fl));
	}

	function parseExpr(funcStart:Bool=false) {
		var tk = token();
		#if debug trace("parseExpr("+tk+")"); #end
		switch( tk ) {
		case TId(id):
			var e = parseStructure(id);
			if( e == null )
				e = EIdent(id);
			return parseExprNext(e);
		case TConst(c):
			return parseExprNext(EConst(c));
		case TPOpen:
			var e = parseExpr();
			ensure(TPClose);
			return parseExprNext(EParent(e));
		case TBrOpen:
			tk = token();
			#if debug trace("parseExpr: "+tk); #end
			switch( tk ) {
			case TBrClose:
				if(funcStart) return EBlock([]);
				return parseExprNext(EObject([]));
			case TId(_),TConst(_):
				var tk2 = token();
				add(tk2);
				add(tk);
				switch( tk2 ) {
				case TDoubleDot:
					return parseExprNext(parseObject());
				default:
				}
			default:
				add(tk);
			}
			var a = new Array();
			while( !opt(TBrClose) ) {
				var e = parseFullExpr();
				a.push(e);
			}
			return EBlock(a);
		case TOp(op):
			if( op.charAt(0) == "/" ) {
				var str = op.substr(1);
				var c = nextChar();
				while( c != "/".code ) {
					str += String.fromCharCode(c);
					c = readChar();
				}
				c = readChar();
				var opts = "";
				while( c >= "a".code && c <= "z".code ) {
					opts += String.fromCharCode(c);
					c = readChar();
				}
				this.char = c;
				return parseExprNext(ERegexp(str, opts));
			}
			var found;
			for( x in unopsPrefix )
				if( x == op )
					return makeUnop(op, parseExpr());
			if( op == "<" )
				return EXML(readXML());
			return unexpected(tk);
		case TBkOpen:
			var a = new Array();
			tk = token();
			while( tk != TBkClose ) {
				add(tk);
				a.push(parseExpr());
				tk = token();
				if( tk == TComma )
					tk = token();
			}
			return parseExprNext(EArrayDecl(a));
		case TComment(s,b):
			return EComment(s,b);
		default:
			return unexpected(tk);
		}
	}

	function makeUnop( op, e ) {
		return switch( e ) {
		case EBinop(bop,e1,e2): EBinop(bop,makeUnop(op,e1),e2);
		default: EUnop(op,true,e);
		}
	}

	function makeBinop( op, e1, e ) {
		return switch( e ) {
		case EBinop(op2, e2, e3):
			var p1 = opPriority.get(op);
			var p2 = opPriority.get(op2);
			if( p1 < p2 || (p1 == p2 && op.charCodeAt(op.length-1) != "=".code) )
				EBinop(op2,makeBinop(op,e1,e2),e3);
			else
				EBinop(op,e1,e);
		default: EBinop(op,e1,e);
		}
	}

	function parseStructure(kwd) {
		#if debug trace("parseStructure(): "+kwd); #end
		return switch( kwd ) {
		case "if":
			ensure(TPOpen);
			var cond = parseExpr();
			ensure(TPClose);
			var e1 = parseExpr();
			end();
			var e2 = if( opt(TId("else"), true) ) parseExpr() else null;
			EIf(cond,e1,e2);
		case "var", "const":
			var vars = [];
			while( true ) {
				var name = id(), t = null, val = null;
				if( opt(TDoubleDot) )
					t = parseType();
				if( opt(TOp("=")) )
					val = parseExpr();
				vars.push( { name : name, t : t, val : val } );
				if( !opt(TComma) )
					break;
			}
			EVars(vars);
		case "while":
			ensure(TPOpen);
			var econd = parseExpr();
			ensure(TPClose);
			var e = parseExpr();
			EWhile(econd,e, false);
		case "for":
			if( opt(TId("each")) ) {
				ensure(TPOpen);
				var ev = parseExpr();
				ensure(TId("in"));
				var e = parseExpr();
				ensure(TPClose);
				EForEach(ev, e, parseExpr());
			} else {
				ensure(TPOpen);
				var inits = [];
				if( !opt(TSemicolon) ) {
					var e = parseExpr();
					if( opt(TId("in")) ) {
						var ein = parseExpr();
						ensure(TPClose);
						return EForIn(e, ein, parseExpr());
					}
					if( opt(TComma) ) {
						inits = parseExprList(TSemicolon);
						inits.unshift(e);
					} else {
						ensure(TSemicolon);
						inits = [e];
					}
				}
				var conds = parseExprList(TSemicolon);
				var incrs = parseExprList(TPClose);
				EFor(inits, conds, incrs, parseExpr());
			}
		case "break":
			var label = switch( peek() ) {
			case TId(n): token(); n;
			default: null;
			};
			EBreak(label);
		case "continue": EContinue;
		case "else": unexpected(TId(kwd));
		case "function":
			var name = switch( peek() ) {
			case TId(n): token(); n;
			default: null;
			};
			EFunction(parseFun(),name);
		case "return":
			EReturn(if( peek() == TSemicolon ) null else parseExpr());
		case "new":
			var t = parseType();
			ENew(t,if( opt(TPOpen) ) parseExprList(TPClose) else []);
		case "throw":
			EThrow( parseExpr() );
		case "try":
			var e = parseExpr();
			var catches = new Array();
			while( opt(TId("catch")) ) {
				ensure(TPOpen);
				var name = id();
				ensure(TDoubleDot);
				var t = parseType();
				ensure(TPClose);
				var e = parseExpr();
				catches.push( { name : name, t : t, e : e } );
			}
			ETry(e, catches);
		case "switch":
			ensure(TPOpen);
			var e = EParent(parseExpr());
			ensure(TPClose);
			var def = null, cl = [];
			ensure(TBrOpen);
			while( !opt(TBrClose) ) {
				if( opt(TId("default")) ) {
					ensure(TDoubleDot);
					def = parseCaseBlock();
				} else {
					ensure(TId("case"));
					var val = parseExpr();
					ensure(TDoubleDot);
					var el = parseCaseBlock();
					cl.push( { val : val, el : el } );
				}
			}
			ESwitch(e, cl, def);
		case "do":
			var e = parseExpr();
			ensure(TId("while"));
			var cond = parseExpr();
			EWhile(cond, e, true);
		case "typeof":
			var e = parseExpr();
			switch(e) {
			case EBinop(op, e1, e2):
				//if(op != "==" && op != "!=")
				//	unexpected(TOp(op));
			case EIdent(id):
			default:
				unexpected(TId(Std.string(e)));
			}
			ETypeof(e);
		default:
			null;
		}
	}

	function parseCaseBlock() {
		#if debug trace("parseCaseBlock()"); #end
		var el = [];
		while( true ) {
			var tk = peek(false);
			switch( tk ) {
			case TId(id): if( id == "case" || id == "default" ) break;
			case TBrClose: break;
			default:
			}
			el.push(parseExpr());
			end();
		}
		return el;
	}
	
	function parseExprNext( e1 : Expr ) {
		var tk = token();
		#if debug trace("parseExprNext("+tk+")"); #end
		switch( tk ) {
		case TOp(op):
			for( x in unopsSuffix )
				if( x == op ) {
					if( switch(e1) { case EParent(_): true; default: false; } ) {
						add(tk);
						return e1;
					}
					return parseExprNext(EUnop(op,false,e1));
				}
			return makeBinop(op,e1,parseExpr());
		case TDot:
			tk = token();
			var field = null;
			switch(tk) {
			case TId(id):
				field = id;
				if( opt(TNs) )
					field = field + "::" + this.id();
			case TOp(op):
				if( op != "<" || switch(e1) { case EIdent(v): v != "Vector"; default: true; } ) unexpected(tk);
				var t = parseType();
				ensure(TOp(">"));
				return parseExprNext(EVector(t));
			case TPOpen:
				ensure(TAt);
				var e2 = parseExpr();
				ensure(TPClose);
				return parseExprNext(EE4X(e1, e2));
			case TAt:
				var id = id();
				return parseExprNext(EE4X(e1, EIdent(id)));
			case TDot:
				var id = "."+id();
				return parseExprNext(EE4X(e1, EIdent(id)));
			default: unexpected(tk);
			}
			return parseExprNext(EField(e1,field));
		case TPOpen:
			return parseExprNext(ECall(e1,parseExprList(TPClose)));
		case TBkOpen:
			var e2 = parseExpr();
			tk = token();
			if( tk != TBkClose ) unexpected(tk);
			return parseExprNext(EArray(e1,e2));
		case TQuestion:
			var e2 = parseExpr();
			tk = token();
			if( tk != TDoubleDot ) unexpected(tk);
			var e3 = parseExpr();
			return ETernary(e1, e2, e3);
		case TId(s):
			switch( s ) {
			case "is": return makeBinop("is", e1, parseExpr());
			case "as": return makeBinop("as",e1,parseExpr());
			default:
				add(tk);
				return e1;
			}
		default:
			add(tk);
			return e1;
		}
	}

	function parseExprList( etk ) : Array<Expr> {
		#if debug trace("parseExprList()"); #end
		
		return cast ignoreComments(
			function() {
			var args = new Array();
			if( opt(etk) )
				return args;
			while( true ) {
				args.push(parseExpr());
				var tk = token();
				switch( tk ) {
				case TComma:
				default:
					if( tk == etk ) break;
					unexpected(tk);
				}
			}
			return args;
			}
		);
		
	}

	function readChar() {
		return try input.readByte() catch( e : Dynamic ) 0;
	}

	function readXML() {
		#if debug trace("readXML()"); #end
		var buf = new StringBuf();
		var input = input;
		buf.addChar("<".code);
		buf.addChar(this.char);
		this.char = 0;
		try {
			var prev = 0;
			while(true) {
				var c = input.readByte();
				buf.addChar(c);
				if( c == ">".code ) break;
				prev = c;
			}
			if( prev == "/".code )
				return buf.toString();
			while(true) {
				var c = input.readByte();
				if( c == "<".code ) {
					c = input.readByte();
					if( c == "/".code ) {
						buf.add("</");
						break;
					}
					this.char = c;
					buf.add(readXML());
					continue;
				}
				buf.addChar(c);
			}
			while(true) {
				var c = input.readByte();
				buf.addChar(c);
				if( c == ">".code ) break;
			}
			return buf.toString();
		} catch( e : haxe.io.Eof ) {
			throw EUnterminatedXML;
		}
	}
	
	function readString( until ) {
		#if debug trace("readString()"); #end
		var c;
		var b = new haxe.io.BytesOutput();
		var esc = false;
		var old = line;
		var s = input;
		while( true ) {
			try {
				c = s.readByte();
			} catch( e : Dynamic ) {
				line = old;
				throw EUnterminatedString;
			}
			if( esc ) {
				esc = false;
				switch( c ) {
				case 'n'.code: b.writeByte(10);
				case 'r'.code: b.writeByte(13);
				case 't'.code: b.writeByte(9);
				case "'".code, '"'.code, '\\'.code: b.writeByte(c);
				case '/'.code: b.writeByte(c);
				case "u".code:
					var code;
					try {
						code = s.readString(4);
					} catch( e : Dynamic ) {
						line = old;
						throw EUnterminatedString;
					}
					var k = 0;
					for( i in 0...4 ) {
						k <<= 4;
						var char = code.charCodeAt(i);
						switch( char ) {
						case 48,49,50,51,52,53,54,55,56,57: // 0-9
							k += char - 48;
						case 65,66,67,68,69,70: // A-F
							k += char - 55;
						case 97,98,99,100,101,102: // a-f
							k += char - 87;
						default:
							throw EInvalidChar(char);
						}
					}
					// encode k in UTF8
					if( k <= 0x7F )
						b.writeByte(k);
					else if( k <= 0x7FF ) {
						b.writeByte( 0xC0 | (k >> 6));
						b.writeByte( 0x80 | (k & 63));
					} else {
						b.writeByte( 0xE0 | (k >> 12) );
						b.writeByte( 0x80 | ((k >> 6) & 63) );
						b.writeByte( 0x80 | (k & 63) );
					}
				default:
					b.writeByte(c);
				}
			} else if( c == '\\'.code )
				esc = true;
			else if( c == until )
				break;
			else {
//				if( c == '\n'.code ) line++;
				b.writeByte(c);
			}
		}
		return b.getBytes().toString();
	}

	function peek(ic:Bool=true) : Token {
		var t : Token = null;
		while(true) {
			if( tokens.isEmpty() )
				add(token());
			t = tokens.first();
			switch(t) {
			case TComment(_,_):
				ic ? tokens.pop() : return t;
			default: break;
			}
		}
		return t;
	}
	
	function id() {
		#if debug trace("id()"); #end
		var t = token();
		while(true) {
			switch(t) {
			case TComment(s,b):
			default: break;
			}
			t = token();
		}
		return switch( t ) {
		case TId(i): #if debug trace("\t-> Got " + i); #end i;
		default: unexpected(t);
		}
	}
	
	function nextChar() {
		var char = 0;
		if( this.char == 0 )
			return readChar();
		char = this.char;
		this.char = 0;
		return char;
	}

	function token() : Token {
		if( !tokens.isEmpty() ) {
			if(no_comments) {
				while(true) {
					var t = tokens.pop();
					if(t == null) break;
					switch(t) {
					case TComment(_,_):
					default:
						return t;
					}
				}
			} else return tokens.pop();
		}
		var char = nextChar();
		while( true ) {
			switch( char ) {
			case 0: return TEof;
			case ' '.code,'\t'.code:
			case '\n'.code:
				line++;
			case '\r'.code:
				line++;
				char = nextChar();
				if( char == '\n'.code )
					char = nextChar();
				continue;
			case ';'.code: return TSemicolon;
			case '('.code: return TPOpen;
			case ')'.code: return TPClose;
			case ','.code: return TComma;
			case '.'.code, '0'.code, '1'.code, '2'.code, '3'.code, '4'.code, '5'.code, '6'.code, '7'.code, '8'.code, '9'.code:
				var buf = new StringBuf();
				while( char >= '0'.code && char <= '9'.code ) {
					buf.addChar(char);
					char = nextChar();
				}
				switch( char ) {
				case 'x'.code:
					if( buf.toString() == "0" ) {
						do {
							buf.addChar(char);
							char = nextChar();
						} while( (char >= '0'.code && char <= '9'.code) || (char >= 'A'.code && char <= 'F'.code) || (char >= 'a'.code && char <= 'f'.code) );
						this.char = char;
						return TConst(CInt(buf.toString()));
					}
					this.char = char;
					return TConst(CInt(buf.toString()));
				case 'e'.code:
					if( buf.toString() == '.' ) {
						this.char = char;
						return TDot;
					}
					buf.addChar(char);
					char = nextChar();
					if( char == '-'.code ) {
						buf.addChar(char);
						char = nextChar();
					}
					while( char >= '0'.code && char <= '9'.code ) {
						buf.addChar(char);
						char = nextChar();
					}
					this.char = char;
					return TConst(CFloat(buf.toString()));
				case '.'.code:
					do {
						buf.addChar(char);
						char = nextChar();
					} while( char >= '0'.code && char <= '9'.code );
					this.char = char;
					var str = buf.toString();
					if( str.length == 1 ) return TDot;
					return TConst(CFloat(str));
				default:
					this.char = char;
					return TConst(CInt(buf.toString()));
				}
			case '{'.code: return TBrOpen;
			case '}'.code: return TBrClose;
			case '['.code: return TBkOpen;
			case ']'.code: return TBkClose;
			case '"'.code, "'".code: return TConst( CString(readString(char)) );
			case '?'.code: return TQuestion;
			case ':'.code:
				char = nextChar();
				if( char == ':'.code )
					return TNs;
				this.char = char;
				return TDoubleDot;
			case '@'.code: return TAt;
			case 0xC2: // UTF8-space
				if( nextChar() != 0xA0 )
					throw EInvalidChar(char);
			case 0xEF: // BOM
				if( nextChar() != 187 || nextChar() != 191 )
					throw EInvalidChar(char);
			default:
				if( ops[char] ) {
					var op = String.fromCharCode(char);
					while( true ) {
						char = nextChar();
						if( !ops[char] ) {
							this.char = char;
							return TOp(op);
						}
						op += String.fromCharCode(char);
						if( op == "//" ) {
							var contents : String = "//";
							try {
								while( char != '\r'.code && char != '\n'.code ) {
									char = input.readByte();
									contents += String.fromCharCode(char);
								}
								this.char = char;
							} catch( e : Dynamic ) {
							}
							return no_comments ? token() : TComment(StringTools.trim(contents), false);
						}
						if( op == "/*" ) {
							var old = line;
							var contents : String = "/*";
							try {
								while( true ) {
									while( char != "*".code ) {
										if( char == "\n".code ) {
											line++;
										}
										else if( char == "\r".code ) {
											line++;
											char = input.readByte();
											contents += String.fromCharCode(char);
											if( char == "\n".code ) {
												char = input.readByte();
												contents += String.fromCharCode(char);
											}
											continue;
										}
										char = input.readByte();
										contents += String.fromCharCode(char);
									}
									char = input.readByte();
									contents += String.fromCharCode(char);
									if( char == 47 )
										break;
								}
							} catch( e : Dynamic ) {
								line = old;
								throw EUnterminatedComment;
							}
							return no_comments ? token() : TComment(contents, true);
						}
						if( op == "!=" ) {
							char = nextChar();
							if(String.fromCharCode(char) != "=")
								this.char = char;
						}
						if( op == "==" ) {
							char = nextChar();
							if(String.fromCharCode(char) != "=")
								this.char = char;
						}
						if( !opPriority.exists(op) ) {
							this.char = char;
							return TOp(op.substr(0, -1));
						}
					}
				}
				if( idents[char] ) {
					var id = String.fromCharCode(char);
					while( true ) {
						char = nextChar();
						if( !idents[char] ) {
							this.char = char;
							return TId(id);
						}
						id += String.fromCharCode(char);
					}
				}
				throw EInvalidChar(char);
			}
			char = nextChar();
		}
		return null;
	}

	function constString( c ) {
		return switch(c) {
		case CInt(v): v;
		case CFloat(f): f;
		case CString(s): s; // TODO : escape + quote
		}
	}

	function tokenString( t ) {
		return switch( t ) {
		case TEof: "<eof>";
		case TConst(c): constString(c);
		case TId(s): s;
		case TOp(s): s;
		case TPOpen: "(";
		case TPClose: ")";
		case TBrOpen: "{";
		case TBrClose: "}";
		case TDot: ".";
		case TComma: ",";
		case TSemicolon: ";";
		case TBkOpen: "[";
		case TBkClose: "]";
		case TQuestion: "?";
		case TDoubleDot: ":";
		case TAt: "@";
		case TNs: "::";
		case TComment(s,b): s;
		}
	}

}
