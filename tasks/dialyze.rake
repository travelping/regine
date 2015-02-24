CURDIR = File.dirname(__FILE__)
DEPS_PLT = "#{ CURDIR }/.deps_plt"

task :dialyze do
  sh "dialyzer --fullpath --plt $(#{ DEPS_PLT }) -Wrace_conditions -r ./apps/*/ebin"
end
