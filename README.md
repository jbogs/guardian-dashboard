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

compiler optimizations, validation elision, and the service to connect to during development may be specificed with:
```
boot develop -v -s <local|laptop|aws> -o <none|whitespace|simple|advanced>
```

## Deployment

to deploy the application to the staging environment in a aws bucket, from the project root:
```
boot deploy -e exotic
```

then navigate the browser to:
```
 http://xoticpcgui.s3-website.us-east-2.amazonaws.com/
```

## Packaging

to build the application as part of the larger product:
```
boot package
```

## License

Copyright © 2016, **VigilanceTech.com**

[1]: http://boot-clj.com
[2]: http://localhost:8000
[3]: http://hoplon.io
