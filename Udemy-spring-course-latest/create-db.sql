DROP SCHEMA IF EXISTS `hb-01-one-to-one-uni`;

CREATE SCHEMA `hb-01-one-to-one-uni`;

USE `hb-01-one-to-one-uni`;

SET FOREIGN_KEY_CHECKS = 0;

CREATE TABLE `instructor_detail` (
`id` int not null AUTO_INCREMENT,
`youtube_channel` VARCHAR(128) DEFAULT null,
`hobby` VARCHAR(60) DEFAULT null,
PRIMARY KEY (`id`)
)ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHAR SET=latin1;

CREATE TABLE `instructor` (
`id` int not null AUTO_INCREMENT,
`fisrt_name` VARCHAR(128) DEFAULT null,
`last_name` VARCHAR(128) DEFAULT null,
`email` VARCHAR(128) DEFAULT null,
`instructor_detail_id` int DEFAULT null,
PRIMARY KEY (`id`),
CONSTRAINT `FK_DETAIL` FOREIGN KEY (`id`) REFERENCES `instructor_detail` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION
)ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHAR SET=latin1;

set FOREIGN_KEY_CHECKS = 1;