 void foo() { 
 int a, b, c, d;
    while (a < b 
      && a > c) {
      fun();
    }
    if (a == b) {
      do { // 3, do
        fun();
      } while (d);
    } else if (c == d) {
      while (c > 0) { 
        fun();
      }
      do { 
        fun();
      } while (a);
    }
  }
}
  