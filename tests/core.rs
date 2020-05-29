#[cfg(test)]
mod core_libraries_tests {
    #[test]
	fn basic_math() {
		//let mut env = env::make_with_core_libs();
		// absolute value
		//assert_eq!(values::Value::Number{5}}, eval(env, "abs 5")?);
		//assert_eq!(val::value{val::num{5}}, eval(env, "abs(-5)")?);
	}

    #[test]
	fn combinatorics() {
		//let env = environment::make_with_core_libs();
		//SUBCASE("factorial") {
		//	CHECK(val::value{val::num{120}} == eval(env, "fact 5").value());
		//}
		//SUBCASE("permutations") {
		//	CHECK(val::value{val::num{60}} == eval(env, "nPk(5, 3)").value());
		//}
		//SUBCASE("combinations") {
		//	// Using an epsilon here because of the division.
		//	CHECK(val::value{tok::boolean{true}} == eval(env, "abs(nCk(5, 3) - 10) < 10**-50").value());
		//}
	}
    #[test]
	fn list_operations() {
		// auto env = environment::make_with_core_libs();
		// SUBCASE("len") {
		// 	CHECK(val::value{val::num{0}} == eval(env, "len []").value());
		// 	CHECK(val::value{val::num{3}} == eval(env, "len [1, 2, 3]").value());
		// }
		// SUBCASE("nth") {
		// 	CHECK(!eval(env, "nth([], 0)").has_value());
		// 	CHECK(val::value{val::num{2}} == eval(env, "nth([1, 2, 3], 1)").value());
		// }
		// SUBCASE("append") {
		// 	val::value const expected = val::make_list(4, 3, 2, 1);
		// 	auto const actual = eval(env, "append([1, 2, 3], 4)");
		// 	CHECK(expected == actual.value());
		// }
		// SUBCASE("reverse") {
		// 	val::value const expected = val::make_list(1, 2, 3);
		// 	auto const actual = eval(env, "reverse [1, 2, 3]");
		// 	CHECK(expected == actual.value());
		// }
		// SUBCASE("concat") {
		// 	val::value const expected = val::make_list(4, 3, 2, 1);
		// 	auto const actual = eval(env, "concat([1, 2], [3, 4])");
		// 	CHECK(expected == actual.value());
		// }
		// SUBCASE("insert into non-empty") {
		// 	val::value const expected = val::make_list(3, 2, 1);
		// 	auto const actual = eval(env, "insert([1, 3], 1, 2)");
		// 	CHECK(expected == actual.value());
		// }
		// SUBCASE("insert into empty") {
		// 	val::value const expected = val::make_list(1);
		// 	auto const actual = eval(env, "insert([], 0, 1)");
		// 	CHECK(expected == actual.value());
		// }
		// SUBCASE("remove from middle") {
		// 	val::value const expected = val::make_list(3, 1);
		// 	auto const actual = eval(env, "remove([1, 2, 3], 1)");
		// 	CHECK(expected == actual.value());
		// }
		// SUBCASE("remove from beginning") {
		// 	val::value const expected = val::make_list(3, 2);
		// 	auto const actual = eval(env, "remove([1, 2, 3], 0)");
		// 	CHECK(expected == actual.value());
		// }
		// SUBCASE("map") {
		// 	val::value const expected = val::make_list(9, 4, 1);
		// 	auto const actual = eval(env, "map([1, 2, 3], x -> x^2)");
		// 	CHECK(expected == actual.value());
		// }
		// SUBCASE("reduce") {
		// 	val::value const expected = val::num{6};
		// 	auto const actual = eval(env, "reduce([1, 2, 3], 0, (a, b) -> a + b)");
		// 	CHECK(expected == actual.value());
		// }
		// SUBCASE("flatmap") {
		// 	val::value const expected = val::make_list(3, 3, 2, 2, 1, 1);
		// 	auto const actual = eval(env, "flatmap([1, 2, 3], x -> [x, x])");
		// 	CHECK(expected == actual.value());
		// }
		// SUBCASE("range") {
		// 	val::value const expected = val::make_list(3, 2, 1);
		// 	auto const actual = eval(env, "range(1, 3)");
		// 	CHECK(expected == actual.value());
		// }
	}
}
