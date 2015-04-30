FROM dockerfile/java:oracle-java8
MAINTAINER Alexandre Archambault <alexandre.archambault@gmail.com>


RUN curl -s http://d3kbcqa49mib13.cloudfront.net/spark-1.3.1-bin-hadoop2.6.tgz | tar -xz -C /opt/
RUN cd /opt && ln -s spark-* spark

ENV SPARK_HOME /opt/spark
ENV PATH $PATH:$SPARK_HOME/bin

WORKDIR /root
