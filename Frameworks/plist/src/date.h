#ifndef OAK_DATE_H_AIWA64W2
#define OAK_DATE_H_AIWA64W2

#import <CoreFoundation/CoreFoundation.h>

namespace oak
{
	// the ‘at’ value is seconds relative to Jan 1 2001 00:00:00 GMT
	struct date_t
	{
		date_t (time_t unixTime);
		date_t (std::string const& str);
		date_t (CFAbsoluteTime at = 0) : at(at)    { }

		explicit operator bool () const            { return at != 0; }
		bool operator== (date_t const& rhs) const  { return at == rhs.at; }
		bool operator!= (date_t const& rhs) const  { return at != rhs.at; }
		bool operator< (date_t const& rhs) const   { return at < rhs.at; }
		double operator- (date_t const& rhs) const { return at - rhs.at; }
		date_t operator+ (double rhs) const        { return date_t(at + rhs); }
		date_t operator- (double rhs) const        { return date_t(at - rhs); }
		double value () const                      { return at; }
		time_t time_value () const                 { return 978307200 + round(at); }
		static date_t now ();
	private:
		CFAbsoluteTime at;
	};

	std::string to_s (date_t const& date, std::string const& dateFormat = "%F %T %z");

} /* oak */

#endif /* end of include guard: OAK_DATE_H_AIWA64W2 */
