'use strict';

angular.module('HomeAutomation.Controllers')
    .controller('CalendarController', function ($scope, $http) {
        $scope.eventSources = {
            url: '/events',
            eventDataTransform: function (e) {
                return {
                    id: e.id,
                    start: e.from,
                    end: e.to
                }                
            }
        };

        $scope.addNewEvent = function (start, end) {
            var event = {
                title: '',
                start: start,
                end: end
            };

            var newEvent = {
                target: 'water',
                from: start,
                to: end
            };

            $http.post('/events/new', newEvent).success(function (result) {                                
                $scope.calendar.fullCalendar('unselect');
                $scope.calendar.fullCalendar('refetchEvents');
            });
        };

        $scope.uiConfig = {
            calendar: {
                defaultView: 'agendaWeek',
                height: 750,
                firstDay: 1,
                allDaySlot: false,
                editable: true,
                selectable: true,
                selectHelper: true,
                columnFormat: {
                    week: 'D.MM dddd'
                },
                axisFormat: { 'agenda': 'H:mm' },
                timeFormat: 'H:mm',

                select: $scope.addNewEvent,
                startParam: 'from',
                endParam: 'to'                
            }
        };
    });