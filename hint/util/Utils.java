package hint.util;

public class Utils {

  // We assume strings are prepared by first splitting on space and tab,
  // and ignoring backslash-escaped spaces. Then we remove the escape slashes
  // altogether.
  // The regular expression uses negative lookbehind to match only
  // those spaces that are not preceded by a \
  // Note the double quotation of the slashback: one for java strings, one for regexps.

  public static String[] prepareForExec (String s) {
      int i;
      
      String [] ss = s.split("\\t|(?<!\\\\) ");
      for (i=0; i<ss.length; i++) {
          ss[i] = unbackslash(ss[i]);
      }
      return ss;
  }

  public static String unbackslash (String s) {
      StringBuffer output = new StringBuffer();

      for(int i=0; i < s.length(); i++) {
          if (s.charAt(i) == '\\' && i <= s.length()) {
              i++;
              char c = s.charAt(i);
             
              if (c == '\\' || c == ' ') {
                  output.append(c);
              }
              else {
                  System.out.println("Invalid unescaped character: "+c);
              }
          }
          else {
              output.append(s.charAt(i));
          }
        }
     // System.out.println(""+output);
      
      return output.toString();
/*
      System.out.println(s);
      String s1 = s.replaceAll("\\\\(\\| )","\\1");
      System.out.println(" "+s1);
      return s1;
 */ }
  
}
