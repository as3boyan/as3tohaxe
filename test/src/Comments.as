package {
	public class Comments {
		/** The comments in this function will be dropped **/
		public static function blah(a:string/*yeah*/="hey") {
			var b = new MyClass(
				/* comment 1 */ a,
				f, // isOk
				g
			);
			var object = {
				f : g, // my comment
				/* more */ i: new MyObject()
			}
			// except this one
		}
	}
}
