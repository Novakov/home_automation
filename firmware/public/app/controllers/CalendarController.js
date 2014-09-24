'use strict';

angular.module('HomeAutomation.Controllers')
    .controller('CalendarController', function ($scope) {
        $scope.eventSources = {
            url: '/events'
        };

        $scope.addNewEvent = function (start, end) {
            var event = {
                title: '',
                start: start,
                end:end
            };

            $scope.calendar.fullCalendar('renderEvent', event, true);

            $scope.calendar.fullCalendar('unselect');
        };

        $scope.uiConfig = {
            calendar: {
                defaultView: 'agendaWeek',
                height: 750,
                firstDay: 1,
                allDaySlot: false,
                editable: true,
                selectable: true,
                selectHelper:true,
                columnFormat: {
                    week: 'D.MM dddd'
                },
                axisFormat: { 'agenda': 'H:mm' },
                timeFormat: 'H:mm',

                select: $scope.addNewEvent
            }            
        };
    });