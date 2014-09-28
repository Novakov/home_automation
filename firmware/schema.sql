-- MySQL dump 10.13  Distrib 5.6.21, for Win64 (x86_64)
--
-- Host: localhost    Database: home_automation
-- ------------------------------------------------------
-- Server version	5.6.21-log

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `deleted_recurring`
--

DROP TABLE IF EXISTS `deleted_recurring`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `deleted_recurring` (
  `event_id` int(11) NOT NULL,
  `date` datetime NOT NULL,
  PRIMARY KEY (`event_id`,`date`),
  CONSTRAINT `event` FOREIGN KEY (`event_id`) REFERENCES `events` (`id`) ON DELETE CASCADE ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `deleted_recurring`
--

LOCK TABLES `deleted_recurring` WRITE;
/*!40000 ALTER TABLE `deleted_recurring` DISABLE KEYS */;
/*!40000 ALTER TABLE `deleted_recurring` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `events`
--

DROP TABLE IF EXISTS `events`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `events` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `target` varchar(45) NOT NULL,
  `applicable_from` datetime NOT NULL,
  `applicable_to` datetime NOT NULL,
  `type` varchar(45) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=85 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `events`
--

LOCK TABLES `events` WRITE;
/*!40000 ALTER TABLE `events` DISABLE KEYS */;
/*!40000 ALTER TABLE `events` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Temporary view structure for view `full_events`
--

DROP TABLE IF EXISTS `full_events`;
/*!50001 DROP VIEW IF EXISTS `full_events`*/;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
/*!50001 CREATE VIEW `full_events` AS SELECT 
 1 AS `id`,
 1 AS `type`,
 1 AS `target`,
 1 AS `applicable_from`,
 1 AS `applicable_to`,
 1 AS `start_at`,
 1 AS `end_at`,
 1 AS `time_start`,
 1 AS `time_end`,
 1 AS `recurr_days`*/;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `recurring_event_definition`
--

DROP TABLE IF EXISTS `recurring_event_definition`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `recurring_event_definition` (
  `event_id` int(11) NOT NULL,
  `time_start` datetime NOT NULL,
  `time_end` datetime NOT NULL,
  `recurr_days` varchar(45) NOT NULL,
  PRIMARY KEY (`event_id`),
  CONSTRAINT `event_id` FOREIGN KEY (`event_id`) REFERENCES `events` (`id`) ON DELETE CASCADE ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `recurring_event_definition`
--

LOCK TABLES `recurring_event_definition` WRITE;
/*!40000 ALTER TABLE `recurring_event_definition` DISABLE KEYS */;
/*!40000 ALTER TABLE `recurring_event_definition` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `single_event_definition`
--

DROP TABLE IF EXISTS `single_event_definition`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `single_event_definition` (
  `event_id` int(11) NOT NULL,
  `start_at` datetime NOT NULL,
  `end_at` datetime NOT NULL,
  PRIMARY KEY (`event_id`),
  CONSTRAINT `fk_event` FOREIGN KEY (`event_id`) REFERENCES `events` (`id`) ON DELETE CASCADE ON UPDATE NO ACTION
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `single_event_definition`
--

LOCK TABLES `single_event_definition` WRITE;
/*!40000 ALTER TABLE `single_event_definition` DISABLE KEYS */;
/*!40000 ALTER TABLE `single_event_definition` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tmp`
--

DROP TABLE IF EXISTS `tmp`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `tmp` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=6 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tmp`
--

LOCK TABLES `tmp` WRITE;
/*!40000 ALTER TABLE `tmp` DISABLE KEYS */;
INSERT INTO `tmp` VALUES (1),(2),(3),(4),(5);
/*!40000 ALTER TABLE `tmp` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Final view structure for view `full_events`
--

/*!50001 DROP VIEW IF EXISTS `full_events`*/;
/*!50001 SET @saved_cs_client          = @@character_set_client */;
/*!50001 SET @saved_cs_results         = @@character_set_results */;
/*!50001 SET @saved_col_connection     = @@collation_connection */;
/*!50001 SET character_set_client      = utf8 */;
/*!50001 SET character_set_results     = utf8 */;
/*!50001 SET collation_connection      = utf8_general_ci */;
/*!50001 CREATE ALGORITHM=UNDEFINED */
/*!50013 DEFINER=`root`@`localhost` SQL SECURITY DEFINER */
/*!50001 VIEW `full_events` AS select `e`.`id` AS `id`,`e`.`type` AS `type`,`e`.`target` AS `target`,`e`.`applicable_from` AS `applicable_from`,`e`.`applicable_to` AS `applicable_to`,`se`.`start_at` AS `start_at`,`se`.`end_at` AS `end_at`,`re`.`time_start` AS `time_start`,`re`.`time_end` AS `time_end`,`re`.`recurr_days` AS `recurr_days` from ((`events` `e` left join `single_event_definition` `se` on((`se`.`event_id` = `e`.`id`))) left join `recurring_event_definition` `re` on((`re`.`event_id` = `e`.`id`))) */;
/*!50001 SET character_set_client      = @saved_cs_client */;
/*!50001 SET character_set_results     = @saved_cs_results */;
/*!50001 SET collation_connection      = @saved_col_connection */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2014-09-28 13:36:15
