#! /usr/bin/env ruby 

# ruby make-config-pc.rb > ruby.pc 

require "rbconfig" 
if RUBY_VERSION < "1.9"
   include Config 
else
	include RbConfig 
end
version = CONFIG["ruby_version"] 
arch = CONFIG["arch"] 
rubyhdrdir = CONFIG["rubyhdrdir"]
if rubyhdrdir.nil?
   rubyhdrdir = CONFIG["rubylibdir"]
else
	rubyhdrdir.chomp("/")
end
dldflags = CONFIG["DLDFLAGS"] 
librubyarg = CONFIG["LIBRUBYARG"] 
libs = CONFIG["LIBS"] 

print <<OUT 
Name: Ruby 
Description: Object Oriented Script Language 
Version: #{version} 
URL: http://www.ruby-lang.org 
Cflags: -I#{rubyhdrdir}/#{arch} -I#{rubyhdrdir} 
Libs: #{dldflags} #{librubyarg} #{libs} 
Requires: 
OUT