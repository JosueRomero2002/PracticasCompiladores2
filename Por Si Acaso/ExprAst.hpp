/* ExprAst.hpp.  Generated automatically by treecc */
#ifndef __yy_ExprAst_hpp
#define __yy_ExprAst_hpp

#include <new>

namespace Ast
{

const int AstNode_kind = 1;
const int Program_kind = 2;
const int LineSeq_kind = 3;
const int Expr_kind = 4;
const int Number_kind = 5;
const int Identifier_kind = 6;
const int BinaryExpr_kind = 7;
const int AddExpr_kind = 8;
const int SubExpr_kind = 9;
const int MulExpr_kind = 10;
const int DivExpr_kind = 11;

class AstNode;
class Program;
class LineSeq;
class Expr;
class Number;
class Identifier;
class BinaryExpr;
class AddExpr;
class SubExpr;
class MulExpr;
class DivExpr;

class YYNODESTATE
{
public:

	YYNODESTATE();
	virtual ~YYNODESTATE();

#line 1 "cpp_skel.h"
private:

	struct YYNODESTATE_block *blocks__;
	struct YYNODESTATE_push *push_stack__;
	int used__;
#line 48 "ExprAst.hpp"
private:

	static YYNODESTATE *state__;

public:

	static YYNODESTATE *getState()
		{
			if(state__) return state__;
			state__ = new YYNODESTATE();
			return state__;
		}

public:

	void *alloc(size_t);
	void dealloc(void *, size_t);
	int push();
	void pop();
	void clear();
	virtual void failed();
	virtual const char *currFilename() const;
	virtual long currLinenum() const;

};

class AstNode
{
protected:

	int kind__;
	const char *filename__;
	long linenum__;

public:

	int getKind() const { return kind__; }
	const char *getFilename() const { return filename__; }
	long getLinenum() const { return linenum__; }
	void setFilename(const char *filename) { filename__ = filename; }
	void setLinenum(long linenum) { linenum__ = linenum; }

	void *operator new(size_t);
	void operator delete(void *, size_t);

protected:

	AstNode();

public:


	virtual int isA(int kind) const;
	virtual const char *getKindName() const;

protected:

	virtual ~AstNode();

};

class Program : public AstNode
{
public:

	Program(AstNode * lines);

public:

	AstNode * lines;


	virtual int isA(int kind) const;
	virtual const char *getKindName() const;

protected:

	virtual ~Program();

};

class LineSeq : public AstNode
{
public:

	LineSeq(AstNode * first_line, AstNode * rest_lines);

public:

	AstNode * first_line;
	AstNode * rest_lines;


	virtual int isA(int kind) const;
	virtual const char *getKindName() const;

protected:

	virtual ~LineSeq();

};

class Expr : public AstNode
{
protected:

	Expr();

public:


	virtual int isA(int kind) const;
	virtual const char *getKindName() const;

protected:

	virtual ~Expr();

};

class Number : public Expr
{
public:

	Number(int value);

public:

	int value;


	virtual int isA(int kind) const;
	virtual const char *getKindName() const;

protected:

	virtual ~Number();

};

class Identifier : public Expr
{
public:

	Identifier(string_t name);

public:

	string_t name;


	virtual int isA(int kind) const;
	virtual const char *getKindName() const;

protected:

	virtual ~Identifier();

};

class BinaryExpr : public Expr
{
public:

	BinaryExpr(Expr * left, Expr * right);

public:

	Expr * left;
	Expr * right;


	virtual int isA(int kind) const;
	virtual const char *getKindName() const;

protected:

	virtual ~BinaryExpr();

};

class AddExpr : public BinaryExpr
{
public:

	AddExpr(Expr * left, Expr * right);

public:


	virtual int isA(int kind) const;
	virtual const char *getKindName() const;

protected:

	virtual ~AddExpr();

};

class SubExpr : public BinaryExpr
{
public:

	SubExpr(Expr * left, Expr * right);

public:


	virtual int isA(int kind) const;
	virtual const char *getKindName() const;

protected:

	virtual ~SubExpr();

};

class MulExpr : public BinaryExpr
{
public:

	MulExpr(Expr * left, Expr * right);

public:


	virtual int isA(int kind) const;
	virtual const char *getKindName() const;

protected:

	virtual ~MulExpr();

};

class DivExpr : public BinaryExpr
{
public:

	DivExpr(Expr * left, Expr * right);

public:


	virtual int isA(int kind) const;
	virtual const char *getKindName() const;

protected:

	virtual ~DivExpr();

};



}
#endif
