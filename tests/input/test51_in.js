class A {
static function main () {
    var a = 10;
    var b = 20;

    function bmethod() {
      var b = 30;
      return a + b;
    }

    function cmethod() {
      var a = 40;
      return bmethod() + a + b;
    }

    var bb = 5;
    return cmethod() + a + bb;
  }
}
