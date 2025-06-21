# miniJava
This is a subset of Java. Imprementing its interpreter, I deepen my understanding.

## miniJava1
### What is to implement
- [x] declaration
- [x] substitution
- [x] single class 
- [x] method with none argument
- [x] int and boolean value
- [x] binary operation
- [x] if statement
- [x] while statement
- [x] input from one file
- [x] simple method calling
- [x] `out` is abbreviation of `System.out.println`
- [x] type checking before executing

### Example
```java=
class Test {
	void main() {
		int x;
		x = x + 1;
		out(x);
		if (x == 0) {
			x = x - 2;
		} else if (x == 1) {
			x = x + 1;
		} else if (x == 2) {
			x = x + 3;
		} else {
			x = 100;
		}
		out(x);

		show();

		int y;
		while (y<100) {
			y = y + 1;
		}
		out(y);
	}

	void show() {
		out(3);
		out(true);
		out(2525252);
	}
}
```