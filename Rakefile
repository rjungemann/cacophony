task :build do
  sh 'mkdir -p build'
  sh 'raco exe -o build/cacophony cacophony.rkt'
end

task :deps do
  sh 'yes | raco pkg install --skip-installed rx osc scribble'
end

task :run do
  sh 'racket cacophony.rkt'
end

task :setup do
  sh 'raco setup'
end

task :default => [:run]
