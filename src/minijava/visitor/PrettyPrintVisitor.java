package minijava.visitor;

import java.io.PrintWriter;

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
import minijava.ast.VarDecl.Kind;
import minijava.util.IndentingWriter;

/**
 * This is an adaptation of the PrettyPrintVisitor from the textbook
 * online material, but updated to work with the "modernized" 
 * Visitor and our own versions of the AST classes.
 * <p>
 * This version is also cleaned up to actually produce *properly* indented
 * output.
 * 
 * @author kdvolder
 */
public class PrettyPrintVisitor implements Visitor<Void> {

	/**
	 * Where to send out.print output.
	 */
	private IndentingWriter out;
	
	public PrettyPrintVisitor(PrintWriter out) {
		this.out = new IndentingWriter(out);
	}
	
	///////////// Visitor methods /////////////////////////////////////////

	@Override
	public Void visit(Program n) {
		n.mainClass.accept(this);
		for ( int i = 0; i < n.classes.size(); i++ ) {
			out.println();
			n.classes.elementAt(i).accept(this);
		}
		return null;
	}

	@Override
	public Void visit(MainClass n) {
		out.print("class "+n.className);
		out.println(" {"); 
		out.indent();
			out.print("public static void main (String [] " + n.argName);
			out.println(") {");
			out.indent();
				n.statement.accept(this);
			out.outdent();
			out.println("}");
		out.outdent();
		out.println("}");
		return null;
	}

	@Override
	public Void visit(ClassDecl n) {
		out.print("class "+n.name);
		if (n.superName!=null)
			out.print(" extends " + n.superName );
		out.println(" { ");
		out.indent();
		for ( int i = 0; i < n.vars.size(); i++ ) {
			n.vars.elementAt(i).accept(this);
		}
		for ( int i = 0; i < n.methods.size(); i++ ) {
			out.println();
			n.methods.elementAt(i).accept(this);
		}
		out.outdent();
		out.println("}");
		return null;
	}

	@Override
	public Void visit(VarDecl n) {
		n.type.accept(this);
		out.print(" " + n.name);
		if (n.kind!=Kind.FORMAL)
			out.println(";");
		return null;
	}

	@Override
	public Void visit(MethodDecl n) {
		out.print("public ");
		n.returnType.accept(this);
		out.print(" " + n.name);
		out.print(" (");
		for ( int i = 0; i < n.formals.size(); i++ ) {
			n.formals.elementAt(i).accept(this);
			if (i+1 < n.formals.size()) { out.print(", "); }
		}
		out.println(") { ");
		out.indent();
		for ( int i = 0; i < n.vars.size(); i++ ) {
			n.vars.elementAt(i).accept(this);
		}
		for ( int i = 0; i < n.statements.size(); i++ ) {
			n.statements.elementAt(i).accept(this);
		}
		out.print("return ");
		n.returnExp.accept(this);
		out.println(";");
		out.outdent();
		out.println("}");
		return null;
	}

	@Override
	public Void visit(IntArrayType n) {
		out.print("int []");
		return null;
	}

	@Override
	public Void visit(BooleanType n) {
		out.print("boolean");
		return null;
	}

	@Override
	public Void visit(IntegerType n) {
		out.print("int");
		return null;
	}

	@Override
	public Void visit(ObjectType n) {
		out.print(n.name);
		return null;
	}

	@Override
	public Void visit(Block n) {
		out.println("{");
		out.indent();
		for ( int i = 0; i < n.statements.size(); i++ ) {
			n.statements.elementAt(i).accept(this);
		}
		out.outdent();
		out.println("}");
		return null;
	}

	@Override
	public Void visit(If n) {
		out.print("if (");
		n.tst.accept(this);
		out.print(") ");
		n.thn.accept(this);
		out.print("else ");
		n.els.accept(this);
		return null;
	}

	@Override
	public Void visit(While n) {
		out.print("while (");
		n.tst.accept(this);
		out.print(") ");
		n.body.accept(this);
		return null;
	}

	@Override
	public Void visit(Print n) {
		out.print("System.out.println(");
		n.exp.accept(this);
		out.println(");");
		return null;
	}

	@Override
	public Void visit(Assign n) {
		out.print(n.name + " = ");
		n.value.accept(this);
		out.println(";");
		return null;
	}

	@Override
	public Void visit(ArrayAssign n) {
		out.print(n.name+"[");
		n.index.accept(this);
		out.print("] = ");
		n.value.accept(this);
		out.println(";");
		return null;
	}

	@Override
	public Void visit(And n) {
		out.print("(");
		n.e1.accept(this);
		out.print(" && ");
		n.e2.accept(this);
		out.print(")");
		return null;
	}

	@Override
	public Void visit(LessThan n) {
		out.print("(");
		n.e1.accept(this);
		out.print(" < ");
		n.e2.accept(this);
		out.print(")");
		return null;
	}

	@Override
	public Void visit(Plus n) {
		out.print("(");
		n.e1.accept(this);
		out.print(" + ");
		n.e2.accept(this);
		out.print(")");
		return null;
	}

	@Override
	public Void visit(Minus n) {
		out.print("(");
		n.e1.accept(this);
		out.print(" - ");
		n.e2.accept(this);
		out.print(")");
		return null;
	}

	@Override
	public Void visit(Times n) {
		out.print("(");
		n.e1.accept(this);
		out.print(" * ");
		n.e2.accept(this);
		out.print(")");
		return null;
	}

	@Override
	public Void visit(ArrayLookup n) {
		n.array.accept(this);
		out.print("[");
		n.index.accept(this);
		out.print("]");
		return null;
	}

	@Override
	public Void visit(ArrayLength n) {
		n.array.accept(this);
		out.print(".length");
		return null;
	}

	@Override
	public Void visit(Call n) {
		n.receiver.accept(this);
		out.print("."+n.name);
		out.print("(");
		for ( int i = 0; i < n.rands.size(); i++ ) {
			n.rands.elementAt(i).accept(this);
			if ( i+1 < n.rands.size() ) { out.print(", "); }
		}
		out.print(")");
		return null;
	}

	@Override
	public Void visit(IntegerLiteral n) {
		out.print(""+n.value);
		return null;
	}

	@Override
	public Void visit(BooleanLiteral n) {
		out.print(""+n.value);
		return null;
	}

	@Override
	public Void visit(IdentifierExp n) {
		out.print(n.name);
		return null;
	}

	@Override
	public Void visit(This n) {
		out.print("this");
		return null;
	}

	@Override
	public Void visit(NewArray n) {
		out.print("new int [");
		n.size.accept(this);
		out.print("]");
		return null;
	}

	@Override
	public Void visit(NewObject n) {
		out.print("new ");
		out.print(n.typeName);
		out.print("()");
		return null;
	}

	@Override
	public Void visit(Not n) {
		out.print("!");
		n.e.accept(this);
		return null;
	}

	@Override
	public <T extends AST> Void visit(NodeList<T> nodes) {
		out.print("NodeList(");
		for (int i = 0; i < nodes.size(); i++) {
			if (i>0) out.print(" , ");
			nodes.elementAt(i).accept(this);
		}
		out.print(")");
		return null;
	}
}
