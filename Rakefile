desc 'Check for Build'
task default: [:get_deps, :update_deps, :build]

desc 'Check for code quality'
task test: [:spec, :eunit, :setup_dialyzer, :dialyze]

Dir.glob('tasks/*.rake').each { |each| import each }
