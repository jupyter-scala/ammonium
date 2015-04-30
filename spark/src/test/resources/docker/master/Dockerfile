FROM test_spark/base
MAINTAINER Alexandre Archambault <alexandre.archambault@gmail.com>

EXPOSE 7077 8080

CMD "$SPARK_HOME/sbin/start-master.sh" -p 7077 --webui-port 8080 && tail -f "$SPARK_HOME/logs/"*
