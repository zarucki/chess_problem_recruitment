#!/bin/bash
SBT_OPTS="-Xms4g -Xmx8g -Xss2M -XX:+CMSClassUnloadingEnabled -XX:MaxMetaspaceSize=1024M" sbt test "$@"
