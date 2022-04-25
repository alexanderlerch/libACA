/* xen support procedures */

#include <mus-config.h>
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <math.h>
#include <time.h>

#ifdef _MSC_VER
  #include <io.h>
  #include <process.h>
  #pragma warning(disable: 4244)
#endif

#if WITH_READLINE
  #include <readline/readline.h>
  #include <readline/history.h>
#endif

#include "xen.h"

#define S_gc_off "gc-off"
#define S_gc_on  "gc-on"


char *xen_strdup(const char *str)
{
  char *newstr = NULL;
  if ((!str) || (!(*str))) return(NULL);
  newstr = (char *)malloc(strlen(str) + 1);
  if (newstr) strcpy(newstr, str);
  return(newstr);
}



/* ------------------------------ RUBY ------------------------------ */

#if HAVE_RUBY

#define HAVE_RB_PROC_NEW 1
/* As the README says, only versions of ruby 1.8 or later will work */

#if USE_SND
void snd_rb_raise(XEN type, XEN info); /* XEN_ERROR */
#endif

#define S_add_help "add_help"
#define S_get_help "get_help"

XEN rb_documentation(XEN name)
{
  XEN_ASSERT_TYPE((XEN_STRING_P(name) || XEN_SYMBOL_P(name)), name, XEN_ONLY_ARG, S_get_help, "a char* or symbol");
  if (XEN_SYMBOL_P(name))
    return(rb_property(XEN_SYMBOL_TO_STRING(name), XEN_DOCUMENTATION_SYMBOL));
  else
    return(rb_property(name, XEN_DOCUMENTATION_SYMBOL));
}


XEN rb_set_documentation(XEN name, XEN help)
{
  XEN_ASSERT_TYPE((XEN_STRING_P(name) || XEN_SYMBOL_P(name)), name, XEN_ARG_1, S_add_help, "a char* or symbol");
  XEN_ASSERT_TYPE(XEN_STRING_P(help), help, XEN_ARG_2, S_add_help, "a char*");
  if (XEN_SYMBOL_P(name))
    rb_set_property(XEN_SYMBOL_TO_STRING(name), XEN_DOCUMENTATION_SYMBOL, help);
  else
    rb_set_property(name, XEN_DOCUMENTATION_SYMBOL, help);
  return(name);
}


static XEN g_add_help(XEN name, XEN help)
{
#define H_add_help S_add_help "(name, help)  add help to topic or function name (String or Symbol)"
  return(rb_set_documentation(name, help));
}


static XEN g_get_help(XEN name)
{
#define H_get_help S_get_help "([name=:" S_get_help "])  \
return help associated with name (String or Symbol) or false"
  if (XEN_NOT_BOUND_P(name))
    return(C_TO_XEN_STRING(H_get_help));
  else
    return(rb_documentation(name));
}


void xen_initialize(void)
{
#ifdef RUBY_INIT_STACK
  RUBY_INIT_STACK;
#endif

  ruby_init();
  ruby_init_loadpath();
  ruby_script("xen");        /* necessary in ruby 1.9 (else segfault in rb_raise -- is this the rb GC bug (see snd-xen.c)?) */

  Init_Hook();
}


off_t xen_to_c_off_t_or_else(XEN obj, off_t fallback)
{
  if (XEN_OFF_T_P(obj))
    return(XEN_TO_C_LONG_LONG(obj));
  else
    if (XEN_NUMBER_P(obj))
      return((off_t)XEN_TO_C_DOUBLE(obj));
  return(fallback);
}


off_t xen_to_c_off_t(XEN obj)
{
  return(XEN_TO_C_LONG_LONG(obj));
}


XEN c_to_xen_off_t(off_t val)
{
    return(C_TO_XEN_LONG_LONG(val));
}


int xen_to_c_int_or_else(XEN obj, int fallback)
{
  /* don't want errors about floats with non-zero fractions etc */
  if (XEN_INTEGER_P(obj))
    return(XEN_TO_C_INT(obj));
  else
    if (XEN_NUMBER_P(obj))
      return((int)XEN_TO_C_DOUBLE(obj));
  return(fallback);
}


void xen_gc_mark(XEN val)
{
  rb_gc_mark(val);
}


XEN xen_rb_cdr(XEN val)
{
  if (XEN_CONS_P(val))
    {
      XEN new_list;
      new_list = XEN_COPY_ARG(val);
      rb_ary_delete_at(new_list, 0);
      return(new_list);
    }
  return(val);
}


XEN xen_rb_cons(XEN arg1, XEN arg2)
{
  if (XEN_NULL_P(arg2))
    return(rb_ary_new3(1, arg1));
  if (!(XEN_CONS_P(arg2)))
    return(rb_ary_new3(2, arg1, arg2));
  return(rb_ary_unshift(arg2, arg1)); /* arg2 assumed to be array here in Ruby */
}


XEN xen_rb_cons2(XEN arg1, XEN arg2, XEN arg3)
{
  return(rb_ary_unshift(xen_rb_cons(arg2, arg3), arg1));
}


XEN xen_rb_ary_new_with_initial_element(long num, XEN element)
{
  XEN arr;
  int i;
  arr = rb_ary_new2(num);
  for (i = 0; i < num; i++)
    rb_ary_store(arr, i, element);
  return(arr);
}


XEN xen_set_assoc(XEN key, XEN val, XEN alist)
{
  /* assoc key val in alist so later rb_ary_assoc will find val given key in alist */
  /*
    if array?(alist)
      if array?(item = alist.assoc(key))
        item[1] = val
      else
        alist.push([key, val])
      end
    else
      [[key, val]]
    end
  */
  if (XEN_CONS_P(alist))
    {
      XEN pair;
      pair = rb_ary_assoc(alist, key);
      if (XEN_CONS_P(pair))
	rb_ary_store(pair, 1, val);
      else rb_ary_push(alist, rb_assoc_new(key, val));
      return(alist);
    }
  return(rb_ary_new3(1, rb_assoc_new(key, val)));
}


XEN xen_assoc(XEN key, XEN alist) 
{ 
  if (XEN_CONS_P(alist)) 
    { 
      XEN val; 
      val = rb_ary_assoc(alist, key); 
      if (val != Qnil) 
	return(rb_ary_entry(val, 1)); 
    } 
  return(Qfalse); 
} 


static char *scheme_to_ruby(const char *name)
{
  /* replace any non-alphanumeric except "?" with "_". "?" -> "_p". '->" -> "2" drop "!" */
  char *new_name = NULL;
  int len, i, j;
  len = strlen(name);
  if (len > 0)
    {
      new_name = (char *)calloc(len + 3, sizeof(char)); /* +1 for possible _p, +1 for possible $ */
      for (i = 0, j = 0; i < len; i++)
	{
	  if (isalnum(name[i]))
	    new_name[j++] = name[i];
	  else 
	    {
	      if (name[i] != '!')
		{
		  if ((name[i] == '-') &&
		      (name[i + 1] == '>'))
		    {
		      new_name[j++] = '2';
		      i++;
		    }
		  else
		    {
		      new_name[j++] = '_';
		      if (name[i] == '?')
			new_name[j++] = 'p';
		    }
		}
	    }
	}
    }
  return(new_name);
}


char *xen_scheme_constant_to_ruby(const char *name)
{
  /* upcase 1st char */
  char *new_name;
  new_name = scheme_to_ruby(name);
  new_name[0] = toupper(new_name[0]);
  return(new_name);
}


char *xen_scheme_procedure_to_ruby(const char *name)
{
  char *new_name = NULL;
  int len, i, j;
  len = strlen(name);
  if (len > 0)
    {
      new_name = (char *)calloc(len + 1, sizeof(char));
      for (i = 0, j = 0; i < len; i++)
	{
	  if ((isalnum(name[i])) || (name[i] == '!') || (name[i] == '?'))
	    new_name[j++] = name[i];
	  else 
	    {
	      if ((name[i] == '-') &&
		  (name[i + 1] == '>'))
		{
		  new_name[j++] = '2';
		  i++;
		}
	      else new_name[j++] = '_';
	    }
	}
    }
  return(new_name);
}


char *xen_scheme_global_variable_to_ruby(const char *name)
{
  /* prepend $ */
  char *new_name;
  new_name = scheme_to_ruby(name);
  if (new_name[0] == '_')
    new_name[0] = '$';
  else
    {
      int i, len;
      len = strlen(new_name);
      for (i = len; i > 0; i--)
	new_name[i] = new_name[i - 1];
      new_name[0] = '$';
    }
  return(new_name);
}


/* looks for global variables and constants (functions too?) */

bool xen_rb_defined_p(const char *name)
{
  char *var_name = scheme_to_ruby(name);
  char buf[128];

  if (var_name[0] == '$')
    sprintf(buf, "defined? %s", var_name);
  else sprintf(buf, "defined? $%s", var_name);

  if (XEN_EVAL_C_STRING(buf) != Qnil)
    {
      free(var_name);
      return(true);
    }
  else
    {
      bool val;
      var_name[0] = toupper(var_name[0]);
      val = rb_const_defined(rb_cObject, rb_intern(var_name));
      free(var_name);
      return(val);
    }
}


XEN xen_rb_gv_get(const char *name)
{
  char *temp;
  XEN val;
  temp = xen_scheme_global_variable_to_ruby(name);
  val = rb_gv_get(temp);
  if (temp) free(temp);
  return(val);
}


XEN xen_rb_gv_set(const char *name, XEN new_val)
{
  char *temp;
  XEN val;
  temp = xen_scheme_global_variable_to_ruby(name);
  val = rb_gv_set(temp, new_val);
  if (temp) free(temp);
  return(val);
}


XEN xen_rb_intern(const char *name)
{
  char *temp;
  XEN val;
  temp = xen_scheme_constant_to_ruby(name);
  val = rb_intern(temp);
  if (temp) free(temp);
  return(val);
}


XEN xen_rb_make_keyword(const char *name)
{
  char *temp;
  XEN val;
  temp = xen_scheme_procedure_to_ruby(name);
  val = C_STRING_TO_XEN_SYMBOL(temp);
  if (temp) free(temp);
  return(val);
}


void xen_rb_define(const char *name, XEN value)
{
  char *temp;
  temp = xen_scheme_constant_to_ruby(name);
  rb_define_global_const(temp, value);
  if (temp) free(temp);
}


XEN xen_rb_define_class(const char *name)
{
  char *temp;
  XEN val;
  temp = xen_scheme_constant_to_ruby(name);
  val = rb_define_class(temp, rb_cObject);
  if (temp) free(temp);
  return(val);
}




#ifndef RARRAY_PTR 
  #define RB_ARRAY_PTR(Ary) RARRAY(Ary)->ptr 
  #define RB_ARRAY_LEN(Ary) RARRAY(Ary)->len 
#else 
  #define RB_ARRAY_PTR(Ary) RARRAY_PTR(Ary) 
  #define RB_ARRAY_LEN(Ary) RARRAY_LEN(Ary) 
#endif 
 

int xen_rb_list_length(XEN obj) 
{ 
  if (XEN_VECTOR_P(obj)) 
     return((int)RB_ARRAY_LEN(obj)); 
  if (obj == XEN_EMPTY_LIST) 
    return(0); 
  return(-1); 
} 


/* XEN_CAR, XEN_CADR..., XEN_LIST_REF, XEN_VECTOR_REF */

XEN xen_rb_list_ref(XEN obj, int index)
{
  if (XEN_VECTOR_P(obj))
    return(rb_ary_entry(obj, (long)index));
  return(XEN_EMPTY_LIST);
}


/* XEN_LIST_SET, XEN_VECTOR_SET */

XEN xen_rb_list_set(XEN obj, int index, XEN value)
{
  if (XEN_VECTOR_P(obj))
    rb_ary_store(obj, (long)index, value);
  return(value);
}


char *xen_version(void)
{
  /* there is no macro we can depend on for the version number (its name changes unpredictably),
   *   and ruby/version.h tries to be funny about how unreliable their semi-functional access is.
   *   Maybe use <ruby/version.h> and ruby_version here (a const char*).
   * No, even that doesn't work because there's no way to tell whether version.h exists.
   *   Humph!
   */
  char *buf;
  buf = (char *)calloc(128, sizeof(char));
  snprintf(buf, 128, "%s", "Ruby");
  return(buf);
}


static XEN xen_rb_report_error(XEN nada, XEN err_info)
{
  /* backtrace info: */
  /*    return rb_funcall(err_info, rb_intern("backtrace"), 0); */
  /* which can be an array of strings */

  fprintf(stderr,"error: %s\n", XEN_AS_STRING(err_info));
  return(XEN_FALSE);
}


static char *rb_prompt = NULL;

static XEN xen_rb_rep(XEN ig)
{
  XEN val;
  char *str;
#if WITH_READLINE
  char *line_read = NULL;
  line_read = readline(rb_prompt);
  if ((line_read) && (*line_read))
    {
      add_history(line_read);
      val = xen_rb_eval_string_with_error(line_read);
      str = XEN_AS_STRING(val);
      fprintf(stdout, "%s\n", (str) ? str : "nil");
      free(line_read);
      line_read = NULL;
    }
#else
  size_t size = 512;
  char **buffer = NULL;
  buffer = (char **)calloc(1, sizeof(char *));
  buffer[0] = (char *)calloc(size, sizeof(char));
  fprintf(stdout, rb_prompt);
  fgets(buffer[0], size, stdin);
  val = xen_rb_eval_string_with_error(buffer[0]);
  str = XEN_AS_STRING(val);
  fprintf(stdout, "%s\n", (str) ? str : "nil");
  free(buffer[0]);
  free(buffer);
#endif
  return(ig);
}


void xen_rb_repl_set_prompt(const char *prompt)
{
  if (rb_prompt) free(rb_prompt);
  rb_prompt = xen_strdup(prompt);
}


static XEN xen_rb_rescue(XEN val)
{
  if (!rb_prompt) rb_prompt = xen_strdup(">");
  return(rb_rescue(XEN_PROCEDURE_CAST xen_rb_rep,
		   XEN_FALSE,
		   XEN_PROCEDURE_CAST xen_rb_report_error,
		   XEN_FALSE));
}


void xen_repl(int argc, char **argv)
{
  while (true)
    {
      int status = 0;
      rb_protect(XEN_VALUE_ARG_PROCEDURE_CAST xen_rb_rescue,
		 XEN_FALSE,
		 &status);
      if (status != 0)
	{
	  fprintf(stderr, "%s\n", XEN_AS_STRING(rb_gv_get("$!")));
	  status = 0;
	}
    }
}


XEN xen_rb_eval_string_with_error(const char *str)
{
  int status = 0;
  XEN res;
  res = rb_eval_string_protect(str, &status);
  if (status != 0)
    return(XEN_TO_STRING(rb_gv_get("$!")));
  return(res);
}


XEN xen_rb_load_file_with_error(XEN file)
{
  int status = 0;
  rb_load_protect(file, 0, &status);
  if (status != 0)
    return(XEN_TO_STRING(rb_gv_get("$!")));
  return(XEN_TRUE);
}


XEN xen_rb_add_to_load_path(char *path) 
{ 
 XEN rpath, load_path; 
 rpath = rb_str_new2(path); 
 load_path = rb_gv_get("$:");
 if (XEN_FALSE_P(rb_ary_includes(load_path, rpath))) 
   rb_ary_unshift(load_path, rpath); 
 return(XEN_FALSE); 
} 


static char *lstbuf = NULL;

static char *xen_rb_list_to_s(XEN lst)
{
  int i, len;
  if (lstbuf == NULL) 
    lstbuf = (char *)calloc(512, sizeof(char));
  else lstbuf[0] = '\0';
  len = XEN_LIST_LENGTH(lst);
  for (i = 0; i < len; i++)
    {
      strcat(lstbuf, XEN_AS_STRING(XEN_LIST_REF(lst, i)));
      strcat(lstbuf, " ");
    }
  return(lstbuf);
}


void xen_rb_raise(XEN type, XEN info)
{
  rb_raise(rb_eStandardError, "%s: %s\n", 
	   rb_id2name(type), 
	   xen_rb_list_to_s(info));
}


int xen_rb_required_args(XEN val)
{
  int args;
  args = XEN_TO_C_INT(val);
  if (args == -1) return(1); 
  if (args < 0) return(abs(args + 1));
  return(args);
}


XEN xen_rb_obj_as_string(XEN obj)
{
  int status = 0;
  XEN result;
  result = rb_protect(XEN_VALUE_ARG_PROCEDURE_CAST rb_obj_as_string,
		      obj,
		      &status);
  if (status != 0)
    return(C_TO_XEN_STRING("<invalid object>"));
  return(result);
}


#if HAVE_RB_PROC_NEW

static XEN xen_rb_apply_1(XEN args) 
{ 
  return(rb_apply(XEN_CAR(args), rb_intern("call"), XEN_CADR(args))); 
} 

#else

static XEN xen_rb_apply_1(XEN args)
{
  if (XEN_PROCEDURE_P(XEN_CAR(args))) 
    return(rb_apply(XEN_CAR(args), rb_intern("call"), XEN_CADR(args))); 
  return(rb_apply(rb_mKernel, XEN_CAR(args), XEN_CADR(args))); 
}

#endif


XEN xen_rb_apply(XEN func, XEN args)
{
  XEN val;
  int status = 0;
  val = rb_protect(XEN_VALUE_ARG_PROCEDURE_CAST xen_rb_apply_1,
		   XEN_LIST_2(func, args),
		   &status);
  if (status != 0)
    return(XEN_TO_STRING(rb_gv_get("$!")));
  return(val);
}


static XEN xen_rb_funcall_0_inner(XEN args)
{
  return(rb_funcall(args, rb_intern("call"), 0));
}


XEN xen_rb_funcall_0(XEN func)
{
  XEN val;
  int status = 0;
  val = rb_protect(XEN_VALUE_ARG_PROCEDURE_CAST xen_rb_funcall_0_inner,
		   func,
		   &status);
  if (status != 0)
    return(XEN_TO_STRING(rb_gv_get("$!")));
  return(val);
}


XEN xen_rb_copy_list(XEN val) 
{ 
  if ((val == XEN_EMPTY_LIST) || (!XEN_CONS_P(val)))
    return XEN_EMPTY_LIST; 
  return rb_ary_dup(val); 
} 


XEN xen_rb_str_new2(char *arg)
{
  return(rb_str_new2((arg) ? arg : ""));
}


double xen_rb_to_c_double_or_else(XEN a, double b) 
{
  return(XEN_NUMBER_P(a) ? NUM2DBL(a) : b);
}


int xen_rb_to_c_int_or_else(XEN a, int b) 
{
  if (XEN_INTEGER_P(a)) return(FIX2INT(a));
  if (XEN_NUMBER_P(a)) return((int)(NUM2DBL(a)));
  return(b);
}


/* class Hook */
 
static XEN xen_rb_cHook;

static XEN hook_alloc(XEN klass)
{
  return(Data_Wrap_Struct(klass, 0, 0, 0));
}


#define XEN_CLASS_HOOK_P(Arg)              rb_obj_is_kind_of(Arg, xen_rb_cHook)

bool xen_rb_hook_p(XEN obj)
{
  return(XEN_CLASS_HOOK_P(obj));
}


bool xen_rb_hook_empty_p(XEN obj) 
{ 
  if (XEN_CLASS_HOOK_P(obj)) 
    return(RB_ARRAY_LEN(rb_iv_get(obj, "@procs")) == 0); 
  return(true); 
} 


/*
 * @name = "$name_of_hook"
 * @arity = arity of procedure(s),         default 0
 * @procs = [["named proc1", proc1], ...]
 */

static XEN xen_rb_hook_initialize(int argc, XEN *argv, XEN hook)
{
  XEN name, arity, help;
  rb_scan_args(argc, argv, "12", &name, &arity, &help);
  XEN_ASSERT_TYPE(XEN_STRING_P(name) || XEN_SYMBOL_P(name), name, XEN_ARG_1, __func__, "a char* or symbol");
  if (XEN_SYMBOL_P(name))
    name = XEN_SYMBOL_TO_STRING(name);
  if (arity != Qnil)
    {
      XEN_ASSERT_TYPE(XEN_INTEGER_P(arity), arity, XEN_ARG_2, __func__, "an integer");
    }
  else arity = INT2NUM(0);
  if (help != Qnil)
    {
      XEN_ASSERT_TYPE(XEN_STRING_P(help), help, XEN_ARG_3, __func__, "a char*");
      XEN_SET_OBJECT_HELP(name, help);
    }
  else help = rb_str_new2("");
  rb_iv_set(hook, "@name", name);
  rb_iv_set(hook, "@arity", arity);
  rb_iv_set(hook, "@procs", rb_ary_new());
  return(hook);
}


/*
 * To create a simple hook in C, see xen.h, XEN_DEFINE_SIMPLE_HOOK.
 * To create a global hook variables, see xen_rb_create_hook() below.
 */

XEN xen_rb_hook_c_new(char *name, int arity, char *help)
{
  XEN args[3];
  args[0] = C_TO_XEN_STRING(name);
  args[1] = C_TO_XEN_INT(arity);
  args[2] = C_TO_XEN_STRING(help);
  return(xen_rb_hook_initialize(3, args, hook_alloc(xen_rb_cHook)));
}


/*
  RUBY_RELEASE_DATE < "2004-03-18" ? old : new

  lambda do         end.arity 	    -1     0 !!!
  lambda do ||      end.arity 	     0     0
  lambda do |a|     end.arity 	    -1     1 !!!
  lambda do |*a|    end.arity 	    -1    -1
  lambda do |a, b|  end.arity 	     2     2
  lambda do |a, *b| end.arity 	    -2    -2
  etc.
*/

#ifdef RUBY_VERSION
  #define XEN_RUBY_RELEASE_DATE  RUBY_RELEASE_DATE
#else
  #define XEN_RUBY_RELEASE_DATE  XEN_TO_C_STRING(XEN_EVAL_C_STRING("RUBY_RELEASE_DATE"))
#endif

#define RUBY_NEW_ARITY_DATE   "2004-03-18"
#define OLD_RUBY_ARITY()      (strcmp(XEN_RUBY_RELEASE_DATE, RUBY_NEW_ARITY_DATE) < 0)
/* #define NEW_RUBY_ARITY()      (strcmp(XEN_RUBY_RELEASE_DATE, RUBY_NEW_ARITY_DATE) >= 0) */

bool xen_rb_arity_ok(int rargs, int args)
{
  if (OLD_RUBY_ARITY())
    {
      if ((rargs >= 2) || (rargs == 0))
	return(rargs == args);
      else if (rargs <= -2)
	return(abs(rargs) <= args);
      else			/* rargs -1 remains (no 1 exists) */
	return((args == 1) || (args == 0) || (args == -1));
    }
  else /* NEW_RUBY_ARITY */
    return((rargs >= 0) ? (rargs == args) : (abs(rargs) <= args));
}

 
static XEN xen_rb_hook_add_hook(int argc, XEN *argv, XEN hook)
{
  XEN name, func;
  int args;
  args = XEN_TO_C_INT(rb_iv_get(hook, "@arity"));
  rb_scan_args(argc, argv, "1&", &name, &func);
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, __func__, "a char*");
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(func) && xen_rb_arity_ok(XEN_TO_C_INT(XEN_ARITY(func)), args),
		  func, XEN_ARG_2, __func__, "a procedure");
  rb_ary_push(rb_iv_get(hook, "@procs"), rb_ary_new3(2, name, func));
  return(hook);
}


#if HAVE_RB_PROC_NEW

static XEN xen_proc_call(XEN args, XEN id) 
{ 
  return(rb_apply(rb_mKernel, (ID)id, XEN_CONS_P(args) ? args : XEN_LIST_1(args))); 
} 

#if 0
  VALUE rb_proc_new((VALUE (*)(ANYARGS/* VALUE yieldarg[, VALUE procarg] */), VALUE)); 
#endif

static XEN xen_rb_proc_new(const char *name, XEN (*func)(), int arity, const char* doc) 
{ 
  rb_define_module_function(rb_mKernel, name, XEN_PROCEDURE_CAST func, arity); 
  if (doc) C_SET_OBJECT_HELP(name, doc); 
  return(rb_proc_new(XEN_PROCEDURE_CAST xen_proc_call, rb_intern(name))); 
} 


static XEN xen_rb_hook_arity(XEN hook); 

XEN xen_rb_add_hook(XEN hook, VALUE (*func)(), const char *name, const char* doc) 
{ 
  /* called from C, not Ruby, to add a function to a Ruby-side hook */ 
  char *temp; 
  temp = xen_scheme_procedure_to_ruby(name); 
  rb_ary_push(rb_iv_get(hook, "@procs"), rb_ary_new3(2, C_TO_XEN_STRING(temp), xen_rb_proc_new(temp, func, XEN_TO_C_INT(xen_rb_hook_arity(hook)), doc))); 
  if (temp) free(temp); 
  return(hook); 
} 

#else

XEN xen_rb_add_hook(XEN hook, VALUE (*func)(), const char *name, const char* doc) 
{
  /* called from C, not Ruby, to add a function to a Ruby-side hook 
   *   this doesn't work in g++ because it thinks the funcs are invalid:
   *   "error: invalid conversion from 'VALUE (*)(VALUE, VALUE)' to 'VALUE (*)(...)'" (snd-file.c etc)
   */ 
  XEN var; 
  char *temp; 
  temp = xen_scheme_procedure_to_ruby(name); 
  rb_define_module_function(rb_mKernel, temp, XEN_PROCEDURE_CAST func, XEN_TO_C_INT_OR_ELSE(rb_iv_get(hook, "@arity"), 0)); 
  if (doc) C_SET_OBJECT_HELP(temp, doc); 
  var = rb_intern(temp); 
  rb_ary_push(rb_iv_get(hook, "@procs"), rb_ary_new3(2, C_TO_XEN_STRING(temp), var)); 
  if (temp) free(temp); 
  return(hook); 
}

#endif


static XEN xen_rb_hook_remove_hook(XEN hook, XEN name)
{
  XEN ary;
  ary = rb_iv_get(hook, "@procs");
  return(rb_ary_delete(ary, rb_ary_assoc(ary, name)));
}


XEN xen_rb_hook_reset_hook(XEN hook)
{
  if (XEN_CLASS_HOOK_P(hook))
    rb_ary_clear(rb_iv_get(hook, "@procs"));
  return(hook);
}


static XEN xen_rb_hook_names(XEN hook)
{
  XEN ary, ret = Qnil;
  long len;
  ary = rb_iv_get(hook, "@procs");
  len = RB_ARRAY_LEN(ary); 
  if (len > 0)
    {
      long i;
      ret = rb_ary_new2(len);
      for (i = 0; i < len; i++)
	rb_ary_store(ret, i, XEN_VECTOR_REF(XEN_VECTOR_REF(ary, i), 0));
    }
  return(ret);
}


XEN xen_rb_hook_to_a(XEN hook)
{
  XEN ret = Qnil;
  if (XEN_CLASS_HOOK_P(hook))
    {
      XEN ary;
      long len;
      ary = rb_iv_get(hook, "@procs");
      len = XEN_LIST_LENGTH(ary);
      if (len > 0)
	{
	  long i;
	  ret = rb_ary_new2(len);
	  for (i = 0; i < len; i++)
	    rb_ary_store(ret, i, XEN_VECTOR_REF(XEN_VECTOR_REF(ary, i), 1));
	}
    }
  return(ret);
}


static XEN xen_rb_hook_run_hook(XEN hook)
{
  if (RB_ARRAY_LEN(rb_iv_get(hook, "@procs"))) 
    rb_ary_each(xen_rb_hook_to_a(hook));
  return(hook);
}


/*
 * Calls all hook-procedures but returns only the last result; use
 * $var_hook.run_hook { |prc| ret << prc.call(*args) } for collecting
 * results.
 */

static XEN xen_rb_hook_call(int argc, XEN *argv, XEN hook)
{
  XEN result = Qnil, rest, procs;
  rb_scan_args(argc, argv, "*", &rest);
  procs = xen_rb_hook_to_a(hook);
  if (procs != Qnil)
    {
      long i;
      for (i = 0; i < RB_ARRAY_LEN(procs); i++) 
	result = xen_rb_apply(rb_ary_entry(procs, i), rest);
    }
  return(result);
}


static XEN xen_rb_hook_is_empty_p(XEN hook)
{
  return(C_TO_XEN_BOOLEAN(RB_ARRAY_LEN(rb_iv_get(hook, "@procs")) == 0)); 
}

 
static XEN xen_rb_hook_length(XEN hook)
{
  return(C_TO_XEN_INT(RB_ARRAY_LEN(rb_iv_get(hook, "@procs")))); 
}


static XEN xen_rb_hook_name(XEN hook)
{
  return(rb_iv_get(hook, "@name"));
}


static XEN xen_rb_hook_describe(XEN hook)
{
  return(XEN_OBJECT_HELP(xen_rb_hook_name(hook)));
}


static XEN xen_rb_hook_arity(XEN hook)
{
  return(rb_iv_get(hook, "@arity"));
}


static XEN xen_rb_hook_inspect(XEN hook)
{
  XEN str = rb_str_new2("#<Hook name: ");
  rb_str_append(str, rb_inspect(rb_iv_get(hook, "@name")));
  rb_str_cat2(str, ", arity: ");
  rb_str_append(str, rb_inspect(rb_iv_get(hook, "@arity")));
  rb_str_cat2(str, ", procs[");
  rb_str_append(str, rb_inspect(xen_rb_hook_length(hook)));
  rb_str_cat2(str, "]: ");
  rb_str_append(str, rb_inspect(xen_rb_hook_names(hook)));
  rb_str_cat2(str, ">");
  return(str);
}    


/* bil -- added xen_rb_create_hook for XEN_DEFINE_HOOK in xen.h, 13-Jun-05 --
 *   seems to work, but I'm guessing, especially the rb_gv_set line.
 *   I can't use rb_define_variable here, as in the old version, because it takes a pointer
 *   to the new variable, which in this case is a local variable => segfault.
 */

XEN xen_rb_create_hook(char *name, int arity, char *help)
{
  XEN var, hook_name;
  char *temp;
  var = xen_rb_hook_c_new(temp = xen_scheme_global_variable_to_ruby(name), arity, help);
  hook_name = xen_rb_hook_name(var);
  rb_gv_set(XEN_TO_C_STRING(hook_name), var);
  if (temp) free(temp);
  return(var);
}

static int simple_hook_number = 0; 
 

XEN xen_rb_create_simple_hook(int arity) 
{ 
  char *name; 
  XEN hook; 
  name = (char *)calloc(20, sizeof(char)); 
  snprintf(name, 20, "simple_%02d_hook", simple_hook_number++); 
  hook = xen_rb_create_hook(name, arity, NULL); 
  free(name); 
  return(hook); 
} 


/*
 * make_hook(name, arity = 0, help = "", hook_name = nil, &func)
 *
 * make_hook("var_hook")
 *   == $var_hook = Hook.new("var_hook")
 * make_hook("var_hook", 1)
 *   == $var_hook = Hook.new("var_hook", 1)
 * make_hook("var_hook", 1, "help $var_hook")
 *   == $var_hook = Hook.new("var_hook", 1, "help $var_hook")
 * 
 * make_hook("var_hook", 1, "help $var_hook", "1st proc") do |a| ... end
 *   == $var_hook = Hook.new("var_hook", 1, "help $var_hook")
 *      $var_hook.add_hook!("1st proc") do |a| ... end
 */

#ifndef RSTRING_LEN 
  #define RB_STR_LEN(str)                RSTRING(str)->len 
#else 
  #define RB_STR_LEN(str)                RSTRING_LEN(str) 
#endif 

static XEN xen_rb_make_hook(int argc, XEN *argv, XEN klass)
{
  XEN hook = XEN_FALSE, name;
  if (argc > 0 && argc < 4)
    {
      hook = xen_rb_hook_initialize(argc, argv, hook_alloc(xen_rb_cHook));
      if (rb_block_given_p())
	{
	  argv[0] = rb_str_new2("");
	  xen_rb_hook_add_hook(1, argv, hook);
	}
    }
  else if (argc == 4 && rb_block_given_p())
    {
      hook = xen_rb_hook_initialize(3, argv, hook_alloc(xen_rb_cHook));
      argv[0] = argv[3];
      xen_rb_hook_add_hook(1, argv, hook);
    }
  else XEN_ERROR(XEN_ERROR_TYPE("wrong-number-of-args"),
		 XEN_LIST_1(C_TO_XEN_STRING("make_hook(name, arity=0, help=\"\", hook_name=\"\", &func)")));
  name = xen_rb_hook_name(hook);
  if (XEN_TO_C_CHAR(name) != '$') 
    {
      char *temp;
      temp = xen_scheme_global_variable_to_ruby(XEN_TO_C_STRING(name)); 
      name = C_TO_XEN_STRING(temp);
      if (temp) free(temp);
    }
  XEN_ASSERT_TYPE(RB_STR_LEN(name) >= 2, name, XEN_ARG_1, __func__, "a char*, len >= 2"); 
  return(rb_gv_set(XEN_TO_C_STRING(name), hook)); 
}


static XEN xen_rb_is_hook_p(XEN klass, XEN obj)
{
  return(C_TO_XEN_BOOLEAN(XEN_CLASS_HOOK_P(obj)));
}


/*
 * Hook.new(name, arity = 0, help = "")
 *
 * $my_hook = Hook.new("my_hook", 2, "info of my_hook")
 * $my_hook.add_hook!("1st proc") do |a, b| ... end
 *     or make_hook("my_hook", 2, "info of my_hook", "1st proc") do |a, b| ... end
 * 
 * $my_hook.add_hook!("2nd proc") do |a, b| ... end
 * $my_hook.inspect   --> #<Hook name: "$my_hook", arity: 2, procs[2]: ["1st proc", "2nd proc"]>
 *
 * ret = 0
 * $my_hook.run_hook do |prc| ret = prc.call(ret, 2) end
 *
 * $my_hook.help      --> info of my_hook
 * $my_hook.remove_hook!("1st proc")
 * $my_hook.inspect   --> #<Hook name: "$my_hook", arity: 2, procs[1]: ["2nd proc"]>
 * 
 * $my_hook.remove_hook!("2nd proc")
 * $my_hook.inspect   --> #<Hook name: "$my_hook", arity: 2, procs[0]: nil>
 */

#if (!HAVE_RB_DEFINE_ALLOC_FUNC)
static XEN xen_rb_new(int argc, XEN *argv, XEN klass)
{
  XEN hook = hook_alloc(klass);
  rb_obj_call_init(hook, argc, argv);
  return(hook);
}
#endif


static XEN rb_object_properties = XEN_FALSE;

#define S_property       "property"
#define S_set_property   "set_property"
#define S_properties     "properties"


XEN rb_property(XEN obj, XEN key)
{
#define H_property S_property "(obj, key)  \
if key exists, return obj's value (maybe nil) associated with key otherwise false"
  XEN props = XEN_FALSE;
  
  if (XEN_FALSE_P(rb_object_properties))
    return(XEN_FALSE);

  props = rb_hash_aref(rb_object_properties, obj);

  if (XEN_FALSE_P(props) || props == Qnil)
    return(XEN_FALSE);
  else
    return(rb_hash_aref(props, key));
}


XEN rb_set_property(XEN obj, XEN key, XEN value)
{
#define H_set_property S_set_property "(obj, key, value)  \
set key-value pair for obj and return value"
  XEN props = XEN_FALSE;

  if (XEN_FALSE_P(rb_object_properties))
    {
      rb_object_properties = rb_hash_new();
      XEN_PROTECT_FROM_GC(rb_object_properties);
    }
  else
    props = rb_hash_aref(rb_object_properties, obj);
  
  if (XEN_FALSE_P(props) || props == Qnil)
    props = rb_hash_new();
  
  rb_hash_aset(props, key, value);
  rb_hash_aset(rb_object_properties, obj, props);
  return(value);
}


XEN rb_properties(void)
{
#define H_properties S_properties "()  return all properties of rb_object_properties (a hash)"
  return(rb_object_properties);
}


static XEN g_gc_off(void) 
{
  #define H_gc_off "(" S_gc_off ") turns off garbage collection"
  rb_gc_disable();
  return(XEN_FALSE);
}


static XEN g_gc_on(void) 
{
  #define H_gc_on "(" S_gc_on ") turns on garbage collection"
  rb_gc_enable();
  return(XEN_FALSE);
}


XEN_ARGIFY_1(g_get_help_w, g_get_help);
XEN_NARGIFY_2(g_add_help_w, g_add_help);
XEN_NARGIFY_3(g_set_property_w, rb_set_property);
XEN_NARGIFY_2(g_property_w, rb_property);
XEN_NARGIFY_0(g_properties_w, rb_properties);

XEN_NARGIFY_0(g_gc_off_w, g_gc_off)
XEN_NARGIFY_0(g_gc_on_w, g_gc_on)


static bool hook_inited = false;

void Init_Hook(void)
{
  if (hook_inited) return;
  hook_inited = true;
 
  xen_rb_cHook = rb_define_class("Hook", rb_cObject);
  rb_include_module(xen_rb_cHook, rb_mEnumerable);
#if HAVE_RB_DEFINE_ALLOC_FUNC
  rb_define_alloc_func(xen_rb_cHook, hook_alloc);
#else
  rb_define_singleton_method(xen_rb_cHook, "new", XEN_PROCEDURE_CAST xen_rb_new, -1);
#endif
    
  rb_define_method(xen_rb_cHook, "initialize", XEN_PROCEDURE_CAST xen_rb_hook_initialize, -1);
  rb_define_method(xen_rb_cHook, "add_hook!", XEN_PROCEDURE_CAST xen_rb_hook_add_hook, -1);
  rb_define_method(xen_rb_cHook, "remove_hook!", XEN_PROCEDURE_CAST xen_rb_hook_remove_hook, 1);
  rb_define_method(xen_rb_cHook, "reset_hook!", XEN_PROCEDURE_CAST xen_rb_hook_reset_hook, 0);
  rb_define_alias(xen_rb_cHook, "clear", "reset_hook!");
  rb_define_method(xen_rb_cHook, "to_a", XEN_PROCEDURE_CAST xen_rb_hook_to_a, 0);
  rb_define_method(xen_rb_cHook, "run_hook", XEN_PROCEDURE_CAST xen_rb_hook_run_hook, 0);
  rb_define_alias(xen_rb_cHook, "each", "run_hook");
  rb_define_method(xen_rb_cHook, "call", XEN_PROCEDURE_CAST xen_rb_hook_call, -1);
  rb_define_method(xen_rb_cHook, "length", XEN_PROCEDURE_CAST xen_rb_hook_length, 0);
  rb_define_alias(xen_rb_cHook, "size", "length");
  rb_define_method(xen_rb_cHook, "empty?", XEN_PROCEDURE_CAST xen_rb_hook_is_empty_p, 0);
  rb_define_method(xen_rb_cHook, "name", XEN_PROCEDURE_CAST xen_rb_hook_name, 0);
  rb_define_method(xen_rb_cHook, "arity", XEN_PROCEDURE_CAST xen_rb_hook_arity, 0);
  rb_define_method(xen_rb_cHook, "describe", XEN_PROCEDURE_CAST xen_rb_hook_describe, 0);
  rb_define_alias(xen_rb_cHook, "help", "describe");
  rb_define_alias(xen_rb_cHook, "documentation", "describe");
  rb_define_method(xen_rb_cHook, "inspect", XEN_PROCEDURE_CAST xen_rb_hook_inspect, 0);
  
  rb_define_global_function("make_hook", XEN_PROCEDURE_CAST xen_rb_make_hook, -1);
  rb_define_global_function("hook?", XEN_PROCEDURE_CAST xen_rb_is_hook_p, 1);

  XEN_DEFINE_PROCEDURE(S_get_help,             g_get_help_w,             0, 1, 0, H_get_help);
  XEN_DEFINE_PROCEDURE(S_add_help,             g_add_help_w,             2, 0, 0, H_add_help);

  XEN_DEFINE_PROCEDURE(S_set_property,         g_set_property_w,         3, 0, 0, H_set_property);
  XEN_DEFINE_PROCEDURE(S_property,             g_property_w,             2, 0, 0, H_property);
  XEN_DEFINE_PROCEDURE(S_properties,           g_properties_w,           0, 0, 0, H_properties);

  XEN_DEFINE_PROCEDURE(S_gc_off,               g_gc_off_w,               0, 0, 0, H_gc_off);
  XEN_DEFINE_PROCEDURE(S_gc_on,                g_gc_on_w,                0, 0, 0, H_gc_on);
}

/* end of class Hook */

#endif



/* ------------------------------ FORTH ------------------------------ */

#if HAVE_FORTH

char *xen_version(void)
{
  return(fth_format("Fth: %s, Xen: " XEN_VERSION, FTH_VERSION));
}


void xen_gc_mark(XEN val)
{
  fth_gc_mark(val);
}


/*
 * A simple interpreter:
 *
 *  #include <xen.h>
 *  
 *  int main(int argc, char **argv)
 *  {
 *    xen_repl(argc, argv);
 *    return(0);
 *  }
 *
 * linking requires xen.o and -lfth -lm
 */

void xen_repl(int argc, char **argv)
{
  fth_repl(argc, argv);
}


off_t xen_to_c_off_t_or_else(XEN obj, off_t fallback)
{
  if (XEN_NUMBER_P(obj))
    return(fth_long_long_ref(obj));
  return(fallback);
}


off_t xen_to_c_off_t(XEN obj)
{
  return(fth_long_long_ref(obj));
}


XEN c_to_xen_off_t(off_t obj)
{
  return(fth_make_long_long(obj));
}


static ficlWord *snd_exit_xt; 

static void fth_snd_exit(int n) 
{ 
  if (!snd_exit_xt) 
    snd_exit_xt = ficlSystemLookup(FTH_FICL_SYSTEM(), "snd-exit"); 
  ficlStackPushInteger(FTH_FICL_STACK(), n); 
  ficlVmExecuteXT(FTH_FICL_VM(), snd_exit_xt); 
  ficlStackDrop(FTH_FICL_STACK(), 1); 
} 
 

static XEN g_gc_off(void) 
{
  #define H_gc_off "(" S_gc_off ") turns off garbage collection"
  fth_gc_on();
  return(XEN_FALSE);
}


static XEN g_gc_on(void) 
{
  #define H_gc_on "(" S_gc_on ") turns on garbage collection"
  fth_gc_on();
  return(XEN_FALSE);
}


void xen_initialize(void)
{
  fth_init();
  fth_exit_hook = fth_snd_exit; 

  XEN_DEFINE_PROCEDURE(S_gc_off, g_gc_off, 0, 0, 0, H_gc_off);
  XEN_DEFINE_PROCEDURE(S_gc_on,  g_gc_on,  0, 0, 0, H_gc_on);
}

#endif 	/* HAVE_FORTH */



/* ------------------------------ S7 ------------------------------ */

#if HAVE_SCHEME
#include "s7.h"

s7_scheme *s7;
XEN xen_false, xen_true, xen_nil, xen_undefined, xen_zero;
size_t xen_s7_number_location, xen_s7_denominator_location;

char *xen_version(void)
{
  char *buf;
  buf = (char *)calloc(64, sizeof(char));
#if HAVE_SNPRINTF
  snprintf(buf, 64, "s7: %s (%s), Xen: %s", S7_VERSION, S7_DATE, XEN_VERSION);
#else
  sprintf(buf, "s7: %s (%s), Xen: %s", S7_VERSION, S7_DATE, XEN_VERSION);
#endif
  return(buf);
}


static char *xen_s7_repl_prompt = NULL;

void xen_s7_set_repl_prompt(const char *new_prompt)
{
  if (xen_s7_repl_prompt) free(xen_s7_repl_prompt);
  xen_s7_repl_prompt = xen_strdup(new_prompt);
}


#if WITH_READLINE
typedef struct {int loc, size, len; const char *text; const char **matches;} completions;

static bool find_match(const char *symbol_name, void *udata)
{
  completions *data = (completions *)udata;
  if (strncmp(data->text, symbol_name, data->len) == 0)
    {
      if (data->matches == NULL)
	{
	  data->matches = (const char **)calloc(8, sizeof(char *));
	  data->size = 8;
	  data->loc = 0;
	}
      else
	{
	  if (data->loc >= data->size)
	    {
	      int i;
	      data->size *= 2;
	      data->matches = (const char **)realloc(data->matches, data->size * sizeof(char *));
	      for (i = data->loc; i < data->size; i++) data->matches[i] = NULL;
	    }
	}
      data->matches[data->loc] = symbol_name;
      data->loc++;
    }
  return(false);
}

static int complete_loc = 0;
static completions *complete_data = NULL;
static char *copy_match(const char *text, int state)
{
  if (complete_loc < complete_data->loc)
    {
      char *result;
      int len;
      len = strlen(complete_data->matches[complete_loc]);
      result = (char *)calloc(len + 1, sizeof(char));
      memcpy((void *)result, (void *)complete_data->matches[complete_loc], len);
      complete_loc++;
      return(result);
    }
  return(NULL);
}

static char **xen_completion(const char *text, int start, int end)
{
  completions *data; 
  char **matches = NULL;
  /* start and end are indices into the original line buffer, we just want text here */

  if (!text) return(NULL);

  data = (completions *)calloc(1, sizeof(completions));
  data->len = strlen(text);
  data->text = text;
		
  s7_for_each_symbol_name(s7, find_match, (void *)data);
  
  /* now to make readline happy, we apparently have to give it copies of everything! */
  if (data->matches)
    {
      complete_data = data;
      complete_loc = 0;
      matches = rl_completion_matches(text, copy_match);
      free(data->matches);
    }

  free(data);
  complete_data = NULL;
  return(matches);
}

#include <curses.h>
#endif


void xen_repl(int argc, char **argv)
{
#if (WITH_READLINE)
  rl_readline_name = "xen";
  rl_attempted_completion_function = xen_completion;
  rl_initialize(); /* get the current COLS value */
  if (COLS > 80)
    {
      char *str;
      str = (char *)calloc(128, sizeof(char));
      snprintf(str, 128, "(with-environment *stacktrace* (set! code-cols %d) (set! total-cols %d) (set! notes-start-col %d))",
	       COLS - 45, COLS, COLS - 45);
      s7_eval_c_string(s7, str);
      free(str);
    }
#else
  int size = 512;
  bool expr_ok = true;
  char *buffer = NULL;
  buffer = (char *)calloc(size, sizeof(char));
#endif

  while (true)
    {
#if WITH_READLINE
      char *line_read = NULL;

      line_read = readline(xen_s7_repl_prompt);
      if (line_read)
	{
	  char *str;
	  str = line_read;
	  while ((str) && (isspace(str[0]))) str++;
	  if (*str)
	    {
	      int len;
	      s7_pointer result;
	      len = strlen(str);
	      add_history(str);
	      result = XEN_EVAL_C_STRING(str);
	      s7_write(s7, result, s7_current_output_port(s7));   /* use write, not display so that strings are in double quotes */
	      s7_newline(s7, s7_current_output_port(s7));
	    }
	  free(line_read);
	}
#else
      if (expr_ok)
	{
	  fprintf(stdout, "\n%s", xen_s7_repl_prompt);
	  expr_ok = false; /* don't get into an infinite loop if running in the background! */
	}
      if (fgets(buffer, size, stdin) != NULL)
	{
	  /* also, it's possible to get a string of spaces or nulls (? -- not sure what is coming in) if stdin is /dev/null */
	  /*   then if (as in condor) stdout is being saved in a file, we get in an infinite loop storing "snd>" until the disk fills up */
	  int i, len;

	  expr_ok = false;
	  len = strlen(buffer);
	  for (i = 0; i < len; i++)
	    {
	      if (buffer[i] == 0)
		break;
	      if (!isspace(buffer[i]))
		{
		  expr_ok = true;
		  break;
		}
	    }
	  if (expr_ok)
	    {
	      char *temp;
	      temp = (char *)malloc(len + 128);
	      snprintf(temp, len + 128, "(write %s)", buffer);    /* use write, not display so that strings are in double quotes */
	      XEN_EVAL_C_STRING(temp);
	      free(temp);
	    }
	}
#endif
    }
#if (!WITH_READLINE)
  free(buffer);
#endif
}


void xen_s7_ignore(s7_function func) /* squelch compiler warnings */
{
}


XEN xen_define_variable(const char *name, XEN value)
{
  XEN_DEFINE(name, value);
  /* s7_gc_protect(s7, value); */
  /* XEN_DEFINE places value in the global env, so it is already gc protected */
  return(C_STRING_TO_XEN_SYMBOL(name));
}


XEN xen_s7_define_hook(const char *name, XEN value)
{
  s7_define_constant(s7, name, value);
  /* s7_gc_protect(s7, value); -- see above */
  return(value);
}


void xen_gc_mark(XEN val)
{
  s7_mark_object(val);
}


#if !(defined(__GNUC__) && (!(defined(__cplusplus))))
XEN xen_s7_c_to_xen_string(const char *str)
{
  return((str) ? s7_make_string(s7, str) : XEN_FALSE);
}
#endif


static char **constant_names = NULL, **constant_helps = NULL;
static int constant_size = 0, constant_top = -1;

void xen_s7_define_constant(s7_scheme *sc, const char *name, s7_pointer value, const char *help)
{
  /* save doc string */
  constant_top++;
  if (constant_top >= constant_size)
    {
      if (constant_size == 0)
	{
	  constant_size = 128;
	  constant_names = (char **)calloc(constant_size, sizeof(char *));
	  constant_helps = (char **)calloc(constant_size, sizeof(char *));
	}
      else
	{
	  int i;
	  i = constant_size;
	  constant_size += 128;
	  constant_names = (char **)realloc(constant_names, constant_size * sizeof(char *));
	  constant_helps = (char **)realloc(constant_helps, constant_size * sizeof(char *));
	  for (; i < constant_size; i++)
	    {
	      constant_names[i] = NULL;
	      constant_helps[i] = NULL;
	    }
	}
    }
  constant_names[constant_top] = xen_strdup(name);
  constant_helps[constant_top] = xen_strdup(help);
  s7_define_constant(s7, name, value);
}


const char *xen_s7_constant_help(const char *name)
{
  int i;
  if (name)
    {
      for (i = 0; i <= constant_top; i++)
	if (strcmp(name, constant_names[i]) == 0)
	  return(constant_helps[i]);
    }
  return(NULL);
}


XEN xen_set_assoc(s7_scheme *sc, s7_pointer key, s7_pointer val, s7_pointer alist)
{
  /* fixup alist, return it (caller has to make sure it is reflected in its object) */
  /*
     (let ((old-val (assoc key alist)))
       (if old-val
           (progn
              (set-cdr! old-val new-val)
              alist)
	   (cons (cons key new-val) alist)))
  */
  XEN old_val;
  old_val = s7_assoc(sc, key, alist); /* returns #f if nothing found */
  if (old_val == s7_f(sc))
    return(s7_cons(sc, s7_cons(sc, key, val), alist));
  s7_set_cdr(old_val, val);
  return(alist);
}


XEN xen_assoc(s7_scheme *sc, XEN key, XEN alist)
{
  XEN val;
  val = s7_assoc(sc, key, alist);
  if (val != s7_f(sc))
    return(s7_cdr(val));
  return(s7_f(sc));
}


/* add various file functions that everyone else implements */

#ifndef _MSC_VER
  #include <unistd.h>
  #include <sys/time.h>
#endif

#include <sys/stat.h>
#include <fcntl.h>


static XEN g_getpid(void)
{
  #define H_getpid "(getpid) returns the current job's process id"
  return(C_TO_XEN_INT((int)getpid()));
}


#if (!WITH_SYSTEM_EXTRAS)
static bool file_probe(const char *arg)
{
#ifndef _MSC_VER
  return(access(arg, F_OK) == 0);
#else
  int fd;
#ifdef O_NONBLOCK
  fd = open(arg, O_RDONLY, O_NONBLOCK);
#else
  fd = open(arg, O_RDONLY, 0);
#endif
  if (fd == -1) return(false);
  close(fd);
  return(true);
#endif
}


static XEN g_file_exists_p(XEN name)
{
  #define H_file_exists_p "(file-exists? filename): #t if the file exists"
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ONLY_ARG, "file-exists?", "a string");
  return(C_TO_XEN_BOOLEAN(file_probe(XEN_TO_C_STRING(name))));
}


static bool directory_p(const char *filename)
{
#if (defined(_MSC_VER) || __CYGWIN__)
  return(false);
#else
#ifdef S_ISDIR
  struct stat statbuf;
  return((stat(filename, &statbuf) >= 0) &&
	 (S_ISDIR(statbuf.st_mode)));
  return(false);
#endif
#endif
}

static XEN g_directory_p(XEN name)
{
  #define H_directory_p "(directory? filename): #t if filename names a directory"
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ONLY_ARG, "directory?", "a string");
  return(C_TO_XEN_BOOLEAN(directory_p(XEN_TO_C_STRING(name)))); /* snd-file.c l 84 */
}

static XEN g_delete_file(XEN name)
{
  #define H_delete_file "(delete-file filename): deletes the file"
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ONLY_ARG, "delete-file", "a string");
  return(C_TO_XEN_BOOLEAN(unlink(XEN_TO_C_STRING(name))));
}


static XEN g_system(XEN command)
{
  #define H_system "(system command): execute command"
  XEN_ASSERT_TYPE(XEN_STRING_P(command), command, XEN_ONLY_ARG, "system", "a string");
  return(C_TO_XEN_INT(system(XEN_TO_C_STRING(command))));
}


static XEN g_s7_getenv(XEN var) /* "g_getenv" is in use in glib! */
{
  #define H_getenv "(getenv var): return value of environment variable var"
  XEN_ASSERT_TYPE(XEN_STRING_P(var), var, XEN_ONLY_ARG, "getenv", "a string");
  return(C_TO_XEN_STRING(getenv(XEN_TO_C_STRING(var))));
}
#endif



#ifdef _MSC_VER
  #include <direct.h>
#endif

static XEN g_getcwd(void)
{
  #define H_getcwd "(getcwd) returns the name of the current working directory"
  char *buf;
  XEN result = XEN_FALSE;
  buf = (char *)calloc(1024, sizeof(char));
#ifdef _MSC_VER
  if (_getcwd(buf, 1024) != NULL)
#else
  if (getcwd(buf, 1024) != NULL)
#endif
    result = C_TO_XEN_STRING(buf);
  free(buf);
  return(result);
}


static XEN g_strftime(XEN format, XEN tm)
{
  #define H_strftime "(strftime format time) returns a string describing the time: (strftime \"%d-%b %H:%M %Z\" (localtime (current-time)))"
  char *buf;
  XEN result;

  XEN_ASSERT_TYPE(XEN_STRING_P(format), format, XEN_ARG_1, "strftime", "a string");
  XEN_ASSERT_TYPE(XEN_WRAPPED_C_POINTER_P(tm), tm, 2, "strftime", "a localtime struct");

  buf = (char *)calloc(1024, sizeof(char));
  strftime(buf, 1024, XEN_TO_C_STRING(format), (const struct tm *)XEN_UNWRAP_C_POINTER(tm));
  result = C_TO_XEN_STRING(buf);
  free(buf);

  return(result);
}


/* (format #f ";~A~%" (strftime "%d-%b %H:%M %Z" (localtime (current-time)))) */
/* these two need to be compatible with g_file_write_date in snd-file.c */

static XEN g_localtime(XEN tm)
{
  #define H_localtime "(localtime tm) breaks up tm into something suitable for strftime"
  time_t rtime;
  rtime = (time_t)XEN_TO_C_ULONG(tm);
  return(XEN_WRAP_C_POINTER(localtime((time_t *)(&rtime))));
}


static XEN g_current_time(void)
{
  time_t curtime;
  #define H_current_time "(current-time) returns the current time (for localtime and strftime)"
  curtime = time(NULL);
  return(C_TO_XEN_ULONG(curtime));
}


static XEN g_tmpnam(void)
{
  #define H_tmpnam "(tmpnam) returns a new (hopefully unused) tempporary file name"
  #define BUFFER_SIZE 512
  static int file_ctr = 0;
  char *str, *tmpdir = NULL;
  int len;
  XEN result;

  str = (char *)calloc(BUFFER_SIZE, sizeof(char));
  tmpdir = xen_strdup(getenv("TMPDIR"));

#ifdef P_tmpdir
  if (tmpdir == NULL) 
    tmpdir = xen_strdup(P_tmpdir); /* /usr/include/stdio.h */
  if (tmpdir)
    {
      len = strlen(tmpdir);
      if (len > 0)
	{
	  if (tmpdir[len - 1] == '/') tmpdir[len - 1] = 0;
	}
      else
	{
	  free(tmpdir);
	  tmpdir = xen_strdup(".");
	}
    }
#else
  if (tmpdir == NULL) tmpdir = xen_strdup("/tmp");
#endif

  snprintf(str, BUFFER_SIZE, "%s/xen_%d_%d", tmpdir, (int)getpid(), file_ctr++);
  if (tmpdir) free(tmpdir);
  result = C_TO_XEN_STRING(str);
  free(str);
  return(result);
}


static XEN g_ftell(XEN fd)
{
  return(C_TO_XEN_OFF_T(lseek(XEN_TO_C_INT(fd), 0, SEEK_CUR)));
}


static XEN g_gc_off(void) 
{
  #define H_gc_off "(" S_gc_off ") turns off garbage collection"
  s7_gc_on(s7, false);
  return(XEN_FALSE);
}


static XEN g_gc_on(void) 
{
  #define H_gc_on "(" S_gc_on ") turns on garbage collection"
  s7_gc_on(s7, true);
  return(XEN_FALSE);
}



XEN_NARGIFY_0(g_getpid_w, g_getpid)
#if (!WITH_SYSTEM_EXTRAS)
  XEN_NARGIFY_1(g_file_exists_p_w, g_file_exists_p)
  XEN_NARGIFY_1(g_directory_p_w, g_directory_p)
  XEN_NARGIFY_1(g_delete_file_w, g_delete_file)
  XEN_NARGIFY_1(g_s7_getenv_w, g_s7_getenv)
  XEN_NARGIFY_1(g_system_w, g_system)
#endif
XEN_NARGIFY_0(g_getcwd_w, g_getcwd)
XEN_NARGIFY_2(g_strftime_w, g_strftime)
XEN_NARGIFY_1(g_localtime_w, g_localtime)
XEN_NARGIFY_0(g_current_time_w, g_current_time)
XEN_NARGIFY_0(g_tmpnam_w, g_tmpnam)
XEN_NARGIFY_1(g_ftell_w, g_ftell)

XEN_NARGIFY_0(g_gc_off_w, g_gc_off)
XEN_NARGIFY_0(g_gc_on_w, g_gc_on)


s7_scheme *s7_xen_initialize(s7_scheme *sc)
{
  xen_s7_repl_prompt = xen_strdup("> ");
  if (!sc)
    {
      s7 = s7_init();
      if (!s7) 
	{
	  fprintf(stderr, "Can't initialize s7!\n");
	  return(NULL);
	}
    }
  else s7 = sc;

  xen_false = s7_f(s7);
  xen_true = s7_t(s7);
  xen_nil = s7_nil(s7);
  xen_undefined = s7_undefined(s7);
  xen_zero = s7_make_integer(s7, 0);
  s7_gc_protect(s7, xen_zero);
  xen_s7_number_location = s7_number_offset(sc);
  xen_s7_denominator_location = s7_denominator_offset(sc);

  XEN_DEFINE_SAFE_PROCEDURE("getpid",              g_getpid_w,             0, 0, 0, H_getpid);
#if (!WITH_SYSTEM_EXTRAS)
  XEN_DEFINE_SAFE_PROCEDURE("file-exists?",        g_file_exists_p_w,      1, 0, 0, H_file_exists_p);
  XEN_DEFINE_SAFE_PROCEDURE("directory?",          g_directory_p_w,        1, 0, 0, H_directory_p);
  XEN_DEFINE_SAFE_PROCEDURE("delete-file",         g_delete_file_w,        1, 0, 0, H_delete_file);
  XEN_DEFINE_SAFE_PROCEDURE("getenv",              g_s7_getenv_w,          1, 0, 0, H_getenv);
  XEN_DEFINE_SAFE_PROCEDURE("system",              g_system_w,             1, 0, 0, H_system);
#endif
  XEN_DEFINE_SAFE_PROCEDURE("getcwd",              g_getcwd_w,             0, 0, 0, H_getcwd);
  XEN_DEFINE_SAFE_PROCEDURE("strftime",            g_strftime_w,           2, 0, 0, H_strftime);
  XEN_DEFINE_SAFE_PROCEDURE("tmpnam",              g_tmpnam_w,             0, 0, 0, H_tmpnam);
  XEN_DEFINE_SAFE_PROCEDURE("localtime",           g_localtime_w,          1, 0, 0, H_localtime);
  XEN_DEFINE_SAFE_PROCEDURE("current-time",        g_current_time_w,       0, 0, 0, H_current_time);
  XEN_DEFINE_SAFE_PROCEDURE("ftell",               g_ftell_w,              1, 0, 0, "(ftell fd): lseek");
  XEN_DEFINE_SAFE_PROCEDURE(S_gc_off,              g_gc_off_w,             0, 0, 0, H_gc_off);
  XEN_DEFINE_SAFE_PROCEDURE(S_gc_on,               g_gc_on_w,              0, 0, 0, H_gc_on);

  XEN_EVAL_C_STRING("(define (hook-push hook func) \n\
                       \"(hook-push hook func) adds func to hook's function list\" \n\
                       (if (not (memq func (hook-functions hook))) (set! (hook-functions hook) (cons func (hook-functions hook)))))");
  XEN_EVAL_C_STRING("(define (hook-append hook func) \n\
                       \"(hook-append hook func) adds func to the end of hook's function list\" \n\
                       (set! (hook-functions hook) (append (hook-functions hook) (list func))))");
  XEN_EVAL_C_STRING("(define (hook-clear hook) (set! (hook-functions hook) ()))");
  XEN_EVAL_C_STRING("(define (hook-remove hook func) \n\
                       (set! (hook-functions hook)\n\
	                     (let loop ((l (hook-functions hook))\n\
		                        (result '()))\n\
	                       (cond ((null? l) (reverse! result))\n\
		                     ((eq? func (car l)) (loop (cdr l) result))\n\
		                     (else (loop (cdr l) (cons (car l) result)))))))");

  XEN_EVAL_C_STRING("(define load-from-path load)");
  XEN_EVAL_C_STRING("(define (1+ x) \"add 1 to arg\" (+ x 1))");
  XEN_EVAL_C_STRING("(define (1- x) \"subtract 1 from arg\" (- x 1))");
  XEN_EVAL_C_STRING("(defmacro while (whether . body) `(do () ((not ,whether)) ,@body))");
  XEN_EVAL_C_STRING("(define (identity x) \"return arg\" x)");

  return(s7);
}


void xen_initialize(void)
{
  s7_xen_initialize(NULL);
}
#endif




/* ------------------------------ NONE OF THE ABOVE ------------------------------ */

#if (!HAVE_EXTENSION_LANGUAGE)

char *xen_version(void)
{
#if HAVE_XEN_STRDUP
  return(xen_strdup("no extension language"));
#else
  char *buf;
  buf = (char *)calloc(64, sizeof(char));
#if HAVE_SNPRINTF
  snprintf(buf, 64, "no extension language");
#else
  sprintf(buf, "no extension language");
#endif
  return(buf);
#endif
}


void xen_repl(int argc, char **argv)
{
}


void xen_initialize(void)
{
}


void xen_gc_mark(XEN val)
{
}


int xen_to_c_int_or_else(XEN obj, int fallback)
{
  return(fallback);
}


off_t xen_to_c_off_t_or_else(XEN obj, off_t fallback)
{
  return(0);
}


off_t xen_to_c_off_t(XEN obj)
{
  return(0);
}


XEN c_to_xen_off_t(off_t val)
{
  return(XEN_ZERO);
}


void xen_no_ext_lang_check_args(const char *name, int args, int req_args, int opt_args, int rst_args)
{
  if (args > 0) /* nargify -- all are required */
    {
      if (req_args != args)
	fprintf(stderr, "%s: %d required args, but req: %d (opt: %d, rst: %d)\n", name, args, req_args, opt_args, rst_args);
      if (opt_args != 0)
	fprintf(stderr, "%s: all args required, but opt: %d (rst: %d)\n", name, opt_args, rst_args);
      if (rst_args != 0)
	fprintf(stderr, "%s: all args required, but rst: %d\n", name, rst_args);
    }
  else
    {
      if (args != -100) /* vargify -- any ok */
	{
	  args = -args;
	  if (rst_args == 0)
	    {
	      if (req_args + opt_args != args)
		fprintf(stderr, "%s: total args: %d, but req: %d and opt: %d\n", name, args, req_args, opt_args);
	    }
	  else
	    {
	      if (req_args + opt_args > args)
		fprintf(stderr, "%s: has :rest, but req: %d and opt: %d , whereas total: %d\n", name, req_args, opt_args, args);
	    }
	}
    }
}

#endif
