static void func(void) {
   printf("Static Inside ");
}

void func1() {
   printf("Inside ");
}

int func2(void) {
	static int a = 5;
   printf("Inside ");
}

int func3(void) {
	register int a = 5;
   printf("Inside ");
}

void main() {
    printf("main");
}  