
#define MAKE_STR_FROM_S48(s48str, cstr) do {		                        \
		cstr = (char*)calloc(S48_STRING_LENGTH(s48str) + 1, sizeof(char));	\
		s48_copy_string_to_latin_1(s48str, cstr);                           \
	} while(0)

#define VALUE_TO_S48(val, type, out) do {   \
		out = S48_MAKE_VALUE(type);		   \
		S48_SET_VALUE(out, type, val);     \
	} while(0)

