package minijava.visitor;

import minijava.ast.AST;
import minijava.ast.And;
import minijava.ast.ArrayAssign;
import minijava.ast.ArrayLength;
import minijava.ast.ArrayLookup;
import minijava.ast.Assign;
import minijava.ast.Block;
import minijava.ast.BooleanLiteral;
import minijava.ast.BooleanType;
import minijava.ast.Call;
import minijava.ast.ClassDecl;
import minijava.ast.IdentifierExp;
import minijava.ast.If;
import minijava.ast.IntArrayType;
import minijava.ast.IntegerLiteral;
import minijava.ast.IntegerType;
import minijava.ast.LessThan;
import minijava.ast.MainClass;
import minijava.ast.MethodDecl;
import minijava.ast.Minus;
import minijava.ast.NewArray;
import minijava.ast.NewObject;
import minijava.ast.NodeList;
import minijava.ast.Not;
import minijava.ast.ObjectType;
import minijava.ast.Plus;
import minijava.ast.Print;
import minijava.ast.Program;
import minijava.ast.This;
import minijava.ast.Times;
import minijava.ast.VarDecl;
import minijava.ast.While;

/**
 * A modernised version of the Visitor interface, adapted from the textbook's
 * version.
 * <p>
 * Changes: this visitor allows you to return something as a result. 
 * The "something" can be of any particular type, so the Visitor 
 * uses Java generics to express this.
 * 
 * @author kdvolder
 */
public interface Visitor<R> {

	//Lists
	public <T extends AST> R visit(NodeList<T> ns);
	
	//Declarations
	public R visit(Program n);
	public R visit(MainClass n);
	public R visit(ClassDecl n);
	public R visit(VarDecl n);
	public R visit(MethodDecl n);
	
	//Types
	public R visit(IntArrayType n);
	public R visit(BooleanType n);
	public R visit(IntegerType n);
	public R visit(ObjectType n);

	//Statements
	public R visit(Block n);
	public R visit(If n);
	public R visit(While n);
	public R visit(Print n);
	public R visit(Assign n);
	public R visit(ArrayAssign n);
	
	//Expressions
	public R visit(And n);
	public R visit(LessThan n);
	public R visit(Plus n);
	public R visit(Minus n);
	public R visit(Times n);
	public R visit(ArrayLookup n);
	public R visit(ArrayLength n);
	public R visit(Call n);
	public R visit(IntegerLiteral n);
	public R visit(BooleanLiteral n);
	public R visit(IdentifierExp n);
	public R visit(This n);
	public R visit(NewArray n);
	public R visit(NewObject n);
	public R visit(Not not);

}
