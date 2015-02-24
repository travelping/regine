task :setup_dialyzer do
  sh "dialyzer --build_plt --apps erts kernel stdlib mnesia " +
      "compiler syntax_tools runtime_tools crypto tools inets " +
      "ssl webtool public_key observer"
  sh "dialyzer --add_to_plt deps/*/ebin"
end
