
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "scheme48.h"
#include "ffi.c"

#define TIME_BUFFER_SIZE 512

s48_value strptime_for_s48(s48_value scm_str, s48_value scm_fmt)
{
	char *str, *fmt;
	s48_value ret;
	struct tm date;
	
	MAKE_STR_FROM_S48(scm_str, str);
	MAKE_STR_FROM_S48(scm_fmt, fmt);

	char* pos = strptime(str, fmt, &date);
	
	free(str);
	free(fmt);

	if(pos != NULL) {

		// Force times to check if daylight
		// savings is in effect
		date.tm_isdst = -1;
		
		time_t t = mktime(&date);
		return s48_enter_integer(t);
	}

	return S48_FALSE;
}

s48_value strftime_for_s48(s48_value scm_fmt, s48_value scm_time)
{	
	long t;
	struct tm date;
	char out_time[TIME_BUFFER_SIZE];
	char *fmt;

	t = s48_extract_integer(scm_time);
	localtime_r(&t, &date);
	MAKE_STR_FROM_S48(scm_fmt, fmt);

 	strftime(out_time, TIME_BUFFER_SIZE, fmt, &date);

	free(fmt);
	return s48_enter_string_latin_1(out_time);
}

/*
s48_value difftime_for_s48(s48_value scm_time1, s48_value scm_time2)
{
	struct tm t1, t2;

	t1 = S48_EXTRACT_VALUE(scm_time1, struct tm);
	t2 = S48_EXTRACT_VALUE(scm_time2, struct tm);

	double diff = difftime(mktime(&t1), mktime(&t2));

	return s48_enter_double(diff);
}
*/

void s48_on_load()
{
	S48_EXPORT_FUNCTION(strptime_for_s48);
	S48_EXPORT_FUNCTION(strftime_for_s48);
}

void s48_on_reload()
{
}
