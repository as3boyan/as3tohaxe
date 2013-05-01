### as3tohaxe 

AS3 to Haxe conversion tool

This is port of as3tohaxe to Haxe 3

Original code can be found here: https://code.google.com/p/haxe/source/browse/#svn%2Fother%2Fas3hx

===========
Config file
===========

as3hx looks for, or creates, a config file in your home directory
called ".as3hx_config.xml". You can also create one in the directory
you are running as3hx from, which will override the home file.

 * indentChars: (String=\t)
    * The characters used to indent each line
 * newlineChars: (String=\n)
    * The characters used to terminate each line
 * bracesOnNewline: (Bool=true)
    * If set, opening braces will be put on a new line
 * uintToInt: (Bool=false)
    * If set, all uint in flash will be translated to Int
 * vectorToArray: (Bool=false)
    * If set, all Vectors will be changed to Arrays
 * guessCasts: (Bool=true)
    * Cast guessing should likely be on, as it will change
    any MyClass(obj) to a cast(obj, MyClass) call.
 * functionToDynamic: (Bool=false)
    * If set, Function will be translated to Dynamic
 * getterMethods: (String=get%I)
    * Can be set to change the names of getters.
	%I : field name with first letter capitalized
	%i : field name unchanged (ex. get_%i)
 * setterMethods: (String=set%I)
    * Can be set to change the names of setters
 * getterSetterStyle: (String="haxe")
    * "haxe": a field will be created with haxe style setters
	        and getters. (var m(getM,setM))
    * "flash": only native getters and setters will be created
	        using @:getter and @:setter
    * "combined": both will be generated

For the "combined" getterSetterStyle, code output will look
similar to:

    #if flash
    
        @:getter(target) function get_target() : {} {
            return proxy;
        }
        
    #else
    
        override function get_target() : Dynamic {
            return proxy;
        }
        
    #end


=================
Current failures:
=================

================================
E4X:
================================
No E4X is done as of yet. This will fail.
Solution: Comment out the function in the source code, and re-run. 

    for each ( var v:XML in classInfo..*.(
          name() == "variable"
          || ( 
                name() == "accessor"
                // Issue #116 - Make sure accessors are readable
                && attribute( "access" ).charAt( 0 ) == "r" )
             ) 
    )

================================
For Initializations:
================================
The output of 

    if(true) {
    	for(var i:uint = 0; i < 7; i++)
    		val += "0";				
    } else {
    	for(i = 0; i < 8; i++)
    		val += "0";
    }

is 

    if(true) {
    	var i : UInt = 0;
    	while(i < 7) {
    			val += "0";
    			i++;
    	}
    } else {
    	i = 0;
    	while(i < 8) {
    			val += "0";
    			i++;
    	}
    }

As you can see, the scope of "i" in flash is not the same as in haxe, 
so the "else" section will produce Unknown identifier : i. The solution
is to move the "var i : UInt = 0;" outside of the blocks in the generated
code.

This can not be avoided by always creating the i variable, as the code

    for(var i:uint = 0; i < 7; i++)
    	val += "0";				
    for(i = 0; i < 8; i++)
    	val += "0";

would then produce a double initialization of i, also causing a compiler error.
 
    var i : UInt = 0;
    while(i < 7) {
    	val += "0";
    	i++;
    }
    var i = 0;
    while(i < 8)
    {
    	val += "0";
    	i++;
    }
