package minijava.typechecker;

import minijava.ast.Program;
import minijava.util.ImpTable;
import minijava.typechecker.implementation.ClassEntry;

/**
 * An instance of this class represents a correctly TypeChecked minijava program.
 * It may also contain other useful information computed by the type checker during
 * type checking.
 * <p>
 * Note that this class has very few methods since you are supposed to be able to
 * implement it in any way you like. You will probably need to carry over information from
 * the type checker phase into subsequent phases, but we don't want to prescribe
 * exactly how or what is to be carried over. (and you needn't worry about this right now,
 * you can change it later as needed, when implementing the rest of the compiler).
 * <p>
 * In phase 2, you don't even need to implement this class. Simply returning a null pointer
 * will suffice (since phase 2 test don't check anything about the type checker's returned
 * result, they only check whether appropriate TypeCheckerExceptions are being raised.
 */
public class TypeChecked {
    public final Program program;
    public final ImpTable<ClassEntry> classTable;
    
    public TypeChecked(Program program, ImpTable<ClassEntry> classTable) 
    {
        this.program = program;
        this.classTable = classTable;
    }
}
