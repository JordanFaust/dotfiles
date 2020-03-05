lib = File.expand_path('lib', __dir__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'eventable/version'

Gem::Specification.new do |spec|
  spec.name          = 'eventable'
  spec.version       = Eventable::VERSION
  spec.authors       = ['Jordan Faust']
  spec.email         = ['jfaust47@gmail.com']

  spec.summary       = 'A DBUS application for desktop events'
  spec.description   = 'A DBUS application for desktop events'
  spec.homepage      = ''

  # Prevent pushing this gem to RubyGems.org. To allow pushes either set the 'allowed_push_host'
  # to allow pushing to a single host or delete this section to allow pushing to any host.
  if spec.respond_to?(:metadata)
    spec.metadata['allowed_push_host'] = ''
  else
    raise 'RubyGems 2.0 or newer is required to protect against ' \
      'public gem pushes.'
  end

  spec.files         = Dir['lib/**/*.rb']
  spec.bindir        = 'bin'
  spec.executables << 'eventable'
  spec.require_paths = ['lib']

  spec.add_runtime_dependency 'gli', '2.18.0'

  spec.add_dependency 'google-api-client', '~> 0.9'
  spec.add_dependency 'rmail', '~> 1.1'
  spec.add_dependency 'ruby-dbus', '~> 0.15'
  spec.add_dependency 'concurrent-ruby', '~> 1.1.5'
  spec.add_dependency 'octokit', '~> 4.14'

  spec.add_development_dependency 'bundler', '~> 2.0'
  spec.add_development_dependency 'pry', '~> 0.12'
  spec.add_development_dependency 'rake', '~> 10.0'
  spec.add_development_dependency 'rdoc', '~> 6.1'
  spec.add_development_dependency 'rspec', '~> 3.0'
  spec.add_development_dependency 'rubocop', '~> 0.67.2'
  spec.add_development_dependency 'simplecov', '~> 0.16'
  spec.add_development_dependency 'webmock', '~> 3.0'
end
