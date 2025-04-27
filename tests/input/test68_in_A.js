class A {
  var x = 100;

  function add(x) {
    return this.x + x;
  }

  static function main() {
    var b = new B();
    b.f = 55;
    return b;
  }
}


class B {
  var f = 5;
}
