from setuptools import setup

setup(
    name='desktopevents',
    description='various events data for your desktop',
    url='http://github.com/JordanFaust/dotfiles',
    author='JordanFaust',
    scripts=[
        'bin/desktopevents-data',
        'bin/desktopevents-email',
        'bin/desktopevents-meetings',
        'bin/desktopevents-notifications'
    ],
    license='MIT',
    packages=['desktopevents'],
    zip_safe=False
)
