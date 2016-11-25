# Guardian Dashboard

The Guardian dashboard, written with [Hoplon][3].

## Dependencies

- java 1.7+
- [boot][1]

## Development

to start a local development server that rebuilds as you make changes and provides an audible notification when each build is complete, from the project root:
```
boot develop
```

then navigate your browser to
```
http://localhost:7000
```

## Deployment

to deploy the application to the staging environment in a aws bucket, from the project root:
```
boot deploy -e xotic
```

then navigate the browser to:
```
 http://xoticpcgui.s3-website.us-east-2.amazonaws.com/
```

# Mobile Directions

## configuration
to create a local development environment on osx:

install homebrew and add the binary folder to your classpath
```
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew update
brew doctor
```

install git
```
brew install git
```

install node version manager
```
brew install nvm
```

install node and npm, then use the latest version
```
nvm install node
nvm use node
```

install cordova
```
npm install -g cordova
```

## browser development
build the application for the browser
```
boot develop
```

## ios development

build the application for ios
```
boot develop -p ios
```

## android development
install the android sdk
```
brew install android
android sdk
```

| Name                       | Version |
|----------------------------|---------|
| Android SDK Tools          | 24.4.1  |
| Android SDK Platform-tools | 23.1    |
| Android SDK Build-tools    | 23.0.1  |
| Android Support Repository | 25      |
| Android Support Library    | 23.1.1  |
| Google Play Services       | 29      |
| Google Repository          | 24      |

download intel's [hardware accelerated execution manager (haxm)](https://software.intel.com/en-us/android/articles/intel-hardware-accelerated-execution-manager) and install it by opening the dmg file and running the installer.

configure an emulator
```
android avd
```

build the application and run it in the android emulator
```
boot develop -p android
```
## License

Copyright © 2016, **VigilanceTech.com**

[1]: http://boot-clj.com
[2]: http://localhost:8000
[3]: http://hoplon.io
