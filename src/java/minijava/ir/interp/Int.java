package minijava.ir.interp;

public class Int extends Word {
	
	public final int value;

	public Int(int v) { value = v; }
	
	@Override
	public String toString() {
		return ""+value;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + value;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Int other = (Int) obj;
		if (value != other.value)
			return false;
		return true;
	}

	@Override
	public Word add(int x) {
		return new Int(value+x);
	}
	
	@Override
	public Word mul(Word r) {
		return new Int(value * r.asInt());
	}
	
	@Override
	public int asInt() {
		return value;
	}
	
	@Override
	public boolean isLT(Word other) {
		return value<other.asInt();
	}
	
	@Override
	public boolean isEQ(Word r) {
		return value==r.asInt();
	}
}
