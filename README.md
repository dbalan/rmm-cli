## RememberClick (Abandoned)

**Not useful for most people**

This was an attemt to see if I can wrap a http API nicely inside a Monad transformer stack. The API being wrapped is [RememberTheMilk](https://www.rememberthemilk.com/services/api/). 

got to a semi-useful state where I can express API as

```haskell
queryTasks :: Text -> RtmApiM (APIResponse TaskListResp)
queryTasks q = makeRequest (Method "rtm.tasks.getList") [("filter", q)]
```

However, finishing this will take too long: The response structures RTM provides is complete bonkers. They are barely documented, uses nested structures which looks like product of legacy. I am not interested going through this pain just to write yet another CLI (not at the moment)

for e.g: the response from queryTasks is this:

```json
({
  "rsp": {
    "stat": "ok",
    "tasks": {
      "rev": "auuqiw11g1p9ys6c6gvv9c1y7oh8b5e",
      "list": [
        {
          "id": "47673164",
          "taskseries": [
            {
              "id": "454678312",
              "created": "2021-08-15T09:17:48Z",
              "modified": "2021-08-29T10:28:16Z",
              "name": "Plan",
              "source": "js",
              "url": "",
              "location_id": "",
              "tags": [],
              "participants": [],
              "notes": {
                "note": [
                  {
                    "id": "88719938",
                    "created": "2021-08-29T10:28:16Z",
                    "modified": "2021-08-29T10:28:16Z",
                    "title": "",
                    "": "Note"
                  }
                ]
              },
              "task": [
                {
                  "id": "820227508",
                  "due": "2021-08-31T22:00:00Z",
                  "has_due_time": "0",
                  "added": "2021-08-15T09:17:48Z",
                  "completed": "",
                  "deleted": "",
                  "priority": "2",
                  "postponed": "0",
                  "estimate": ""
                }
              ]
            },
            {
...
```

