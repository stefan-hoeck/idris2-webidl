// Extracted from https://w3c.github.io/media-source/#text-track-extensions

enum ReadyState {
    "closed",
    "open",
    "ended"
};

enum EndOfStreamError {
    "network",
    "decode"
};

[Exposed=Window]
interface MediaSource : EventTarget {
    constructor();
    readonly        attribute SourceBufferList    sourceBuffers;
    readonly        attribute SourceBufferList    activeSourceBuffers;
    readonly        attribute ReadyState          readyState;
                    attribute unrestricted double duration;
                    attribute EventHandler        onsourceopen;
                    attribute EventHandler        onsourceended;
                    attribute EventHandler        onsourceclose;
    SourceBuffer   addSourceBuffer (DOMString type);
    undefined           removeSourceBuffer (SourceBuffer sourceBuffer);
    undefined           endOfStream (optional EndOfStreamError error);
    undefined           setLiveSeekableRange (double start, double end);
    undefined           clearLiveSeekableRange ();
    static boolean isTypeSupported (DOMString type);
};

enum AppendMode {
    "segments",
    "sequence"
};

[Exposed=Window]
interface SourceBuffer : EventTarget {
                    attribute AppendMode          mode;
    readonly        attribute boolean             updating;
    readonly        attribute TimeRanges          buffered;
                    attribute double              timestampOffset;
    readonly        attribute AudioTrackList      audioTracks;
    readonly        attribute VideoTrackList      videoTracks;
    readonly        attribute TextTrackList       textTracks;
                    attribute double              appendWindowStart;
                    attribute unrestricted double appendWindowEnd;
                    attribute EventHandler        onupdatestart;
                    attribute EventHandler        onupdate;
                    attribute EventHandler        onupdateend;
                    attribute EventHandler        onerror;
                    attribute EventHandler        onabort;
    undefined appendBuffer (BufferSource data);
    undefined abort ();
    undefined remove (double start, unrestricted double end);
};

[Exposed=Window]
interface SourceBufferList : EventTarget {
    readonly        attribute unsigned long length;
                    attribute EventHandler  onaddsourcebuffer;
                    attribute EventHandler  onremovesourcebuffer;
    getter SourceBuffer (unsigned long index);
};

[Exposed=Window]
partial interface URL {
    static DOMString createObjectURL (MediaSource mediaSource);
};

partial interface AudioTrack {
    readonly        attribute SourceBuffer? sourceBuffer;
};

partial interface VideoTrack {
    readonly        attribute SourceBuffer? sourceBuffer;
};

partial interface TextTrack {
    readonly        attribute SourceBuffer? sourceBuffer;
};
