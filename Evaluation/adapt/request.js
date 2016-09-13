var http = require("http");
var async = require("async");
var proc = require("process");
var fs = require("fs");

var options = {
  hostname: '52.197.58.225',
  port: 80,
  path: '/posts/2014-02-Fanfiction-Graphs-PageRank/img/HP_union_size_larger.png',
  method: 'get',
  headers: {
    'Connection': 'keep-alive',
    'Cache-Control': 'no-cache',
    'Upgrade-Insecure-Requests': 1,
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36',
    'Accept-Encoding': 'gzip, deflate, sdch',
    'Accept-Language': 'en,zh-CN;q=0.8,zh;q=0.6,zh-TW;q=0.4',
  }
};

function estimateTime(results) {
    var ret = {};
    ret.success = 0;
    ret.sucRatio = 0;
    var sucSumTime = 0;
    ret.sucAvgTime = 0;
    ret.normTime = 0;
    for(var i of results) {
        if(i !== null) {
            ret.success += 1;
            sucSumTime += i;
        }
    }
    ret.sucRatio = ret.success / results.length;
    ret.sucAvgTime = sucSumTime / ret.success;
    ret.normTime = ret.sucAvgTime / ret.sucRatio;
    return ret;
}

function simulateRequests(myID, numUsers, allReqCallback) {
    async.times(numUsers, (i, callback) => {
        var start = Date.now();
        var req = http.get(options, (res) => {
            var body = "";
            res.resume();
            res.on('end', () => {
                var elapsed = Date.now() - start;
                callback(null, elapsed);
            });
        });
        req.on('error', (e) => {
            console.log(e.message);
            callback(null, null);
        });
    }, (err, results) => {
        var r = estimateTime(results);
        console.log(`${myID}: ${numUsers} users, average (${r.normTime}), ${results}`);
        allReqCallback({ id: myID, results: results });
    });
}

var schedule = [];

for(var i = 1; i <= 50; i++) {
    schedule.push(i <= 10 ? i : 10);
}
for(var i = 10; i>=1; i--) {
    schedule.push(i);
}

function simulateSchedule( schedule, filename ) {
    var schedResults = [];
    var cb = (n) => {
        simulateRequests(n, schedule[n], (res) => {
            schedResults.push(res);
            if(schedResults.length === schedule.length) {
                console.log("all scheduled requests finished!");
                var allResults = [];
                for(res of schedResults) {
                    allResults = allResults.concat(res.results);
                }
                stat = estimateTime(allResults);
                console.log('Stats:');
                console.log(stat);
                fs.writeFile(filename, JSON.stringify(schedResults));
            }
        });
        if(n < schedule.length - 1) {
            setTimeout(cb, 1000, n + 1);
        }
    };
    setTimeout(cb, 10, 0);
}

simulateSchedule(schedule, proc.argv[2] === undefined ? "request.log" : proc.argv[2]);
