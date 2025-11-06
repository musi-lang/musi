@link("test.func1")
unsafe extern "C" proc func1();

@link("test.func2")
@link("test.alias")
unsafe extern "C" proc func2(x: Int);
