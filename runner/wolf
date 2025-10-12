#!/usr/bin/env ruby
require 'fileutils'

class App
  def initialize
    @source_path = "/home/iso/Documents/GitHub/Wolf-Lang/target/debug/Wolf-Lang"
    @run_file = ARGV[0]
  end

  def run
    unless @run_file&.end_with?(".wolf")
      puts "Please provide a .wolf file path"
      return
    end

    # Ensure Wolf-Lang binary exists in current directory
    unless File.exist?("./Wolf-Lang")
      FileUtils.cp(@source_path, "./Wolf-Lang")
      FileUtils.chmod("+x", "./Wolf-Lang")
    end

    # Construct and run command
    cmd = "./Wolf-Lang --file #{@run_file}"
    system(cmd)
  end
end

App.new.run
