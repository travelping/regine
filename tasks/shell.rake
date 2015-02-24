task :shell do
  sh "erl -pa ebin deps/*/ebin"
end
