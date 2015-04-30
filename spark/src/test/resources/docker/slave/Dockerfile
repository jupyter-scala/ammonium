FROM test_spark/base
MAINTAINER Alexandre Archambault <alexandre.archambault@gmail.com>

EXPOSE 7078 8080

CMD "$SPARK_HOME/bin/spark-class" "org.apache.spark.deploy.worker.Worker" "spark://master:7077" -p 7078 --webui-port 8080 && tail -f "$SPARK_HOME/logs/"*
