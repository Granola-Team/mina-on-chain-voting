task :default => :menu

desc 'Print the menu of targets'
task :menu do
  sh 'rake', '-T'
end

desc 'Clean the web subdirectory'
task :clean_web do
  chdir 'web'
  sh 'pnpm', 'clean'
end
